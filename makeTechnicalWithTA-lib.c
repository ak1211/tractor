//
// TA-libでテクニカル指標を計算するプログラム
// https://ak1211.com
// Copyright (c) 2018 Akihiro Yamamoto
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdalign.h>
#include <math.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include <libgen.h>
#include <linux/limits.h>
#include <assert.h>

#include <ta-lib/ta_libc.h>

typedef int errno_t;

enum {
    DATE_TIME_COLUMN_WIDTH = 24,    // 日付時刻列の幅
    COLUMN_WIDTH_MIN = 9,           // 列の最小幅
    COLUMN_WIDTH_MAX = 128,         // 列の最大幅(多めに)
    ONE_TIME_ROWS = 128,            // 漸進処理1回の行数
};

int max( int a, int b )
{
    return (a > b) ? a : b;
}

//
// double型 v は 0.0 か？
//
bool double_equal_zero( double v )
{
    return ( fabs(v) <= DBL_EPSILON );
}

// 行または列見出し型
typedef char rc_caption_t[COLUMN_WIDTH_MAX+1];
typedef rc_caption_t row_caption_t; // 行見出し型
typedef rc_caption_t col_caption_t; // 列見出し型

// パラメータ型
typedef double param_t;

// 列型
typedef struct {
    col_caption_t col_capt; // 列見出し
    param_t param[3];       // パラメータ(0値は不要を表す)
    int begIdx;             // 入力値のインデックスに対して、
                            // 出力値の始まるインデックス(これ未満はN/A)
    int nbElement;          // 値の数
    double vs[];            // 値
} column_t;

//
// column_t 型の新規割り当て関数
// 返値は使用後にfreeすること
column_t* new_column_t( col_caption_t incol_capt,
                        double inp1,
                        double inp2,
                        double inp3,
                        size_t length )
{
    column_t* p = calloc( 1, sizeof(column_t)+sizeof(double)*length );
    if( p == NULL ) {
        return NULL;
    }

    assert( sizeof(p->param)/sizeof(p->param[0]) == 3 );
    p->param[0] = inp1;
    p->param[1] = inp2;
    p->param[2] = inp3;
    //
    strncpy( p->col_capt, incol_capt, COLUMN_WIDTH_MAX );
    if( !double_equal_zero(p->param[0]) ) {
        // パラメータを書く
        strncat( p->col_capt, "(", COLUMN_WIDTH_MAX );
        for( int i=0; i<sizeof(p->param)/sizeof(p->param[0]); i++ ) {
            // 0を見つけると打ち切る
            if( double_equal_zero(p->param[i]) ) {break;}
            //
            col_caption_t work;
            if( double_equal_zero( p->param[i] - floor(p->param[i]) ) ) {
                // 整数部のみ
                snprintf( work, COLUMN_WIDTH_MAX, "%d,", (int)floor(p->param[i]) );
            } else {
                // 浮動小数
                snprintf( work, COLUMN_WIDTH_MAX, "%.2f,", p->param[i] );
            }
            strncat( p->col_capt, work, COLUMN_WIDTH_MAX );
        }
        // 最終の','を')'に上書きする
        p->col_capt[ strlen(p->col_capt) - 1 ] = ')';
    }
    return p;
}

//
// 文字列を指定の幅で出力する
//
void printString( int colwidth, char* s ) {
    char fmt[COLUMN_WIDTH_MAX+1];
    snprintf( fmt, COLUMN_WIDTH_MAX, ",%%%ds", colwidth );
    printf( fmt, s );
}

//
// double型を指定の幅で出力する
//
void printDouble( int colwidth, double v ) {
    char fmt[COLUMN_WIDTH_MAX+1];
    snprintf( fmt, COLUMN_WIDTH_MAX, ",%%%d.2f", colwidth );
    printf( fmt, double_equal_zero(v) ? 0.00 : v );
}

//
// 行内に列を注入する
//
void inject_column( int idx, const column_t* column )
{
    int colwidth = max( strlen(column->col_capt), COLUMN_WIDTH_MIN );

    if( idx < column->begIdx ) {
        printString( colwidth, "N/A" );
    } else if( idx - column->begIdx < column->nbElement ) {
        printDouble( colwidth, column->vs[idx - column->begIdx] );
    } else {
        printString( colwidth, "N/A" );
    }
}

//
// 計算結果を出力する
//
void disp(  const row_caption_t caption[],
            int startIdx, int endIdx, const double inprice[],
            int columnCount, ... )
{
    va_list ap;
    va_start( ap, columnCount );

    column_t** this_arg = calloc( columnCount, sizeof(column_t*) );
    if( this_arg ) {
        char fmt[COLUMN_WIDTH_MAX+1];
        snprintf( fmt, COLUMN_WIDTH_MAX, "%%%ds", DATE_TIME_COLUMN_WIDTH );
        // タイトル行の出力
        printf( fmt, "DateTime" );
        printString( COLUMN_WIDTH_MIN, "Close" );
         for( int i=0; i<columnCount; i++ ) {
            this_arg[i] = va_arg( ap, column_t* );
            printString( COLUMN_WIDTH_MIN, this_arg[i]->col_capt );
        }
        printf("\n");

        // データ行の出力
        for( int i=startIdx; i<=endIdx; i++ ) {
            printf( fmt, caption[i] );
            printDouble( COLUMN_WIDTH_MIN, inprice[i] );
            for( int k=0; k<columnCount; k++ ) {
                inject_column( i, this_arg[k] );
            }
            printf("\n");
        }
        free( this_arg );
    }
    va_end( ap );
}

//
// TSVから列を分解して取り出す
//
size_t read_part(   FILE* fp,
                    row_caption_t* caption,
                    double* open,
                    double* high,
                    double* low,
                    double* close,
                    double* volume,
                    size_t length )
{
    size_t count=0;
    for( ; count<length; count++ ) {
        row_caption_t date;
        char time[256];
        double o, h, l, c, v;
        char buff[256];
        if( fgets( buff, sizeof(buff), fp) == NULL ) {break;}
        if( sscanf( buff,
                    "%s%s" "%lf%lf%lf%lf%lf" "\n",
                    date, time,
                    &o, &h, &l, &c, &v )
            == EOF)
        {
            break;
        }
        if(errno) {goto fail;}
        // DateTimeを作る
        snprintf( caption[count], COLUMN_WIDTH_MAX, "%sT%s+0900", date, time );
        open[count] = o;
        high[count] = h;
        low[count]  = l;
        close[count]= c;
        volume[count]= v;
    }
    return count;
fail:
    perror("read_part");
    return 0UL;
}

//
// 入力TSVファイルから列を分解して取り出す
//
errno_t read_file(  FILE *fp,
                    row_caption_t** outcaption,
                    double** outopen,
                    double** outhigh,
                    double** outlow,
                    double** outclose,
                    double** outvolume,
                    size_t* outlength )
{
    row_caption_t* capt = NULL;
    double* open = NULL;
    double* high = NULL;
    double* low = NULL;
    double* close = NULL;
    double* volume = NULL;

    assert( fp != NULL );
    // 1,2行目はタイトル行なので読み飛ばす
    if (1) {
        // テキストモードの入力を仮定している
        for (int c=0; c!=EOF && c!='\n'; c=fgetc(fp) ){}
        for (int c=0; c!=EOF && c!='\n'; c=fgetc(fp) ){}
        if(errno) {goto fail;}
    }

    size_t rowcount = 0;
    for( int batchcount=0; !feof(fp); batchcount++ ) {
        const size_t nmemb = (1+batchcount) * ONE_TIME_ROWS;
        void* p = NULL;
        // 処理ごとに領域の拡張を行なう
        if((p=realloc(capt, sizeof(row_caption_t)*nmemb)) == NULL){goto fail;}else{capt=p;}
        if((p=realloc(open, sizeof(double)*nmemb)) == NULL){goto fail;}else{open=p;}
        if((p=realloc(high, sizeof(double)*nmemb)) == NULL){goto fail;}else{high=p;}
        if((p=realloc(low, sizeof(double)*nmemb)) == NULL){goto fail;}else{low=p;}
        if((p=realloc(close, sizeof(double)*nmemb)) == NULL){goto fail;}else{close=p;}
        if((p=realloc(volume, sizeof(double)*nmemb)) == NULL){goto fail;}else{volume=p;}
        //
        rowcount += read_part(  fp,
                                &capt[ batchcount*ONE_TIME_ROWS ],
                                &open[ batchcount*ONE_TIME_ROWS ],
                                &high[ batchcount*ONE_TIME_ROWS ],
                                &low[ batchcount*ONE_TIME_ROWS ],
                                &close[ batchcount*ONE_TIME_ROWS ],
                                &volume[ batchcount*ONE_TIME_ROWS ],
                                ONE_TIME_ROWS );
        if(errno) {goto fail;}
    }

    // 成功結果を返す
    *outcaption = capt;
    *outopen = open;
    *outhigh= high;
    *outlow = low;
    *outclose = close;
    *outvolume = volume;
    *outlength = rowcount;
    return EXIT_SUCCESS;
fail:
    perror("read_file");
    // 失敗したので、この関数で用意した領域を解放する
    free(capt);
    free(open);
    free(high);
    free(low);
    free(close);
    free(volume);
    return EXIT_FAILURE;
}

//
// テクニカル指標の計算と出力
//
TA_RetCode calculate(   row_caption_t* caption,
                        double* open,
                        double* high,
                        double* low,
                        double* close,
                        double* volume,
                        size_t length )
{
    struct columns {
        column_t* sma;
        column_t* ema;
        column_t* wma;
        column_t* dema;
        column_t* tema;
        column_t* trima;
        column_t* kama;
        column_t* mama;
        column_t* t3;
        //
        column_t* ema12;
        column_t* ema26;
        column_t* macd;
        column_t* signal;
        column_t* hist;
        //
        column_t* bbup;
        column_t* bbmid;
        column_t* bblow;
        column_t* rsi;
        column_t* roc;
        column_t* mom;
        column_t* sar;
        column_t* willr;
        //
        column_t* adx;
        column_t* adxr;
        column_t* posDI;
        column_t* negDI;
        //
        column_t* ad;
        column_t* adosc;
        column_t* obv;
    } __attribute__((__packed__)) v = {NULL};   // NULL初期化する
    column_t** columns_pp = (column_t**)&v;     // 上の構造体をポインタの配列で
                                                // アクセスするためのダブルポインタ

    TA_RetCode rc = TA_SUCCESS;
    //
    do {
        if((v.sma   = new_column_t( "SMA", 12,0,0, length )) == NULL) {break;}
        if((v.ema   = new_column_t( "EMA", 12,0,0, length )) == NULL) {break;}
        if((v.wma   = new_column_t( "WMA", 12,0,0, length )) == NULL) {break;}
        if((v.dema  = new_column_t( "DEMA", 12,0,0, length )) == NULL) {break;}
        if((v.tema  = new_column_t( "TEMA", 12,0,0, length )) == NULL) {break;}
        if((v.trima = new_column_t( "TRIMA", 12,0,0, length )) == NULL) {break;}
        if((v.kama  = new_column_t( "KAMA", 12,0,0, length )) == NULL) {break;}
        if((v.mama  = new_column_t( "MAMA", 12,0,0, length )) == NULL) {break;}
        if((v.t3    = new_column_t( "T3", 12,0,0, length )) == NULL) {break;}
        //
        if((v.ema12 = new_column_t( "EMA", 12,0,0, length )) == NULL) {break;}
        if((v.ema26 = new_column_t( "EMA", 26,0,0, length )) == NULL) {break;}
        if((v.macd  = new_column_t( "MACD", 12,26,9, length )) == NULL) {break;}
        if((v.signal= new_column_t( "Signal", 12,26,9, length )) == NULL) {break;}
        if((v.hist  = new_column_t( "Hist", 12,26,9, length )) == NULL) {break;}
        //
        if((v.bbup  = new_column_t( "BBUP", 25,1.5,1.5, length )) == NULL) {break;}
        if((v.bbmid = new_column_t( "BBMID", 25,1.5,1.5, length )) == NULL) {break;}
        if((v.bblow = new_column_t( "BBLOW", 25,1.5,1.5, length )) == NULL) {break;}
        if((v.rsi   = new_column_t( "RSI", 14,0,0, length )) == NULL) {break;}
        if((v.roc   = new_column_t( "ROC", 14,0,0, length )) == NULL) {break;}
        if((v.mom   = new_column_t( "MOM", 14,0,0, length )) == NULL) {break;}
        if((v.sar   = new_column_t( "SAR", 0.04,0.2,0, length )) == NULL) {break;}
        if((v.willr = new_column_t( "Williams%R", 14,0,0, length )) == NULL) {break;}
        //
        if((v.adx   = new_column_t( "ADX",  9,0,0, length )) == NULL) {break;}
        if((v.adxr  = new_column_t( "ADXR", 9,0,0, length )) == NULL) {break;}
        if((v.posDI = new_column_t( "+DI", 14,0,0, length )) == NULL) {break;}
        if((v.negDI = new_column_t( "-DI", 14,0,0, length )) == NULL) {break;}
        //
        if((v.ad    = new_column_t( "Chaikin A/D Line", 0,0,0, length )) == NULL) {break;}
        if((v.adosc = new_column_t( "Chaikin A/D Oscillator", 3,10,0, length )) == NULL) {break;}
        if((v.obv   = new_column_t( "On Balance Volume", 0,0,0, length )) == NULL) {break;}

        rc = TA_Initialize();
        if(rc) {break;}

        // EMAが安定するまでの数
        rc = TA_SetUnstablePeriod( TA_FUNC_UNST_EMA, 20 );
        if(rc) {break;}

        //
        // SMA - Simple Moving Average
        //
        rc = TA_MA( 0, (length-1), close,
                    v.sma->param[0],
                    TA_MAType_SMA,
                    &v.sma->begIdx, &v.sma->nbElement, v.sma->vs );
        if(rc) {break;}

        //
        // EMA - Exponential Moving Average
        //
        rc = TA_MA( 0, (length-1), close,
                    v.ema->param[0],
                    TA_MAType_EMA,
                    &v.ema->begIdx, &v.ema->nbElement, v.ema->vs );
        if(rc) {break;}

        //
        // WMA - Weighted Moving Average
        //
        rc = TA_MA( 0, (length-1), close,
                    v.wma->param[0],
                    TA_MAType_WMA,
                    &v.wma->begIdx, &v.wma->nbElement, v.wma->vs );
        if(rc) {break;}

        //
        // DEMA - Double Exponential Moving Average
        //
        rc = TA_MA( 0, (length-1), close,
                    v.dema->param[0],
                    TA_MAType_DEMA,
                    &v.dema->begIdx, &v.dema->nbElement, v.dema->vs );
        if(rc) {break;}

        //
        // TEMA - Triple Exponential Moving Average
        //
        rc = TA_MA( 0, (length-1), close,
                    v.tema->param[0],
                    TA_MAType_TEMA,
                    &v.tema->begIdx, &v.tema->nbElement, v.tema->vs );
        if(rc) {break;}

        //
        // TRIMA - Triangular Moving Average
        //
        rc = TA_MA( 0, (length-1), close,
                    v.trima->param[0],
                    TA_MAType_TRIMA,
                    &v.trima->begIdx, &v.trima->nbElement, v.trima->vs );
        if(rc) {break;}

        //
        // KAMA - Kaufman Adaptive Moving Average
        //
        rc = TA_MA( 0, (length-1), close,
                    v.kama->param[0],
                    TA_MAType_KAMA,
                    &v.kama->begIdx, &v.kama->nbElement, v.kama->vs );
        if(rc) {break;}

        //
        // MAMA - MESA Adaptive Moving Average
        //
        rc = TA_MA( 0, (length-1), close,
                    v.mama->param[0],
                    TA_MAType_MAMA,
                    &v.mama->begIdx, &v.mama->nbElement, v.mama->vs );
        if(rc) {break;}

        //
        // T3 - Triple Exponential Moving Average (T3)
        //
        rc = TA_MA( 0, (length-1), close,
                    v.t3->param[0],
                    TA_MAType_T3,
                    &v.t3->begIdx, &v.t3->nbElement, v.t3->vs );
        if(rc) {break;}

        //
        // EMA - Exponential Moving Average
        //
        rc = TA_MA( 0, (length-1), close,
                    v.ema12->param[0],
                    TA_MAType_EMA,
                    &v.ema12->begIdx, &v.ema12->nbElement, v.ema12->vs );
        if(rc) {break;}

        //
        // EMA - Exponential Moving Average
        //
        rc = TA_MA( 0, (length-1), close,
                    v.ema26->param[0],
                    TA_MAType_EMA,
                    &v.ema26->begIdx, &v.ema26->nbElement, v.ema26->vs );
        if(rc) {break;}

        //
        // MACD - Moving Average Convergence/Divergence
        //
        // この3値は必ず一致するように設定すること
        assert( 2*v.macd->param[0] - v.hist->param[0] - v.signal->param[0] == 0 );
        assert( 2*v.macd->param[1] - v.hist->param[1] - v.signal->param[1] == 0 );
        assert( 2*v.macd->param[2] - v.hist->param[2] - v.signal->param[2] == 0 );
        //
        rc = TA_MACD( 0, (length-1), close,
                    v.macd->param[0], v.macd->param[1], v.macd->param[2],
                    &v.macd->begIdx, &v.macd->nbElement,
                    v.macd->vs,
                    v.signal->vs,
                    v.hist->vs );
        v.hist->begIdx      = v.signal->begIdx      = v.macd->begIdx;
        v.hist->nbElement   = v.signal->nbElement   = v.macd->nbElement;
        if(rc) {break;}

        //
        // BBANDS - Bollinger Bands
        //
        // この3値は必ず一致するように設定すること
        assert( 2*v.bbup->param[0] - v.bbmid->param[0] - v.bblow->param[0] == 0 );
        assert( 2*v.bbup->param[1] - v.bbmid->param[1] - v.bblow->param[1] == 0 );
        assert( 2*v.bbup->param[2] - v.bbmid->param[2] - v.bblow->param[2] == 0 );
        //
        rc = TA_BBANDS( 0, (length-1), close,
                    v.bbup->param[0],
                    v.bbup->param[1],
                    v.bbup->param[2],
                    TA_MAType_SMA,      // Simple MA
                    &v.bblow->begIdx, &v.bblow->nbElement,
                    v.bbup->vs,
                    v.bbmid->vs,
                    v.bblow->vs );
        v.bbup->begIdx      = v.bbmid->begIdx       = v.bblow->begIdx;
        v.bbup->nbElement   = v.bbmid->nbElement    = v.bblow->nbElement;
        if(rc) {break;}

        //
        // RSI - Relative Strength Index
        //
        rc = TA_RSI( 0, (length-1), close,
                    v.rsi->param[0],
                    &v.rsi->begIdx, &v.rsi->nbElement, v.rsi->vs );
        if(rc) {break;}

        //
        // ROC - Rate of change : ((price/prevPrice)-1)*100
        //
        rc = TA_ROC( 0, (length-1), close,
                    v.roc->param[0],
                    &v.roc->begIdx, &v.roc->nbElement, v.roc->vs );
        if(rc) {break;}

        //
        // MOM - Momentum
        //
        rc = TA_MOM( 0, (length-1), close,
                    v.mom->param[0],
                    &v.mom->begIdx, &v.mom->nbElement, v.mom->vs );
        if(rc) {break;}

        //
        // SAR - Parabolic SAR
        //
        rc = TA_SAR( 0, (length-1), high, low,
                    v.sar->param[0], v.sar->param[1],
                    &v.sar->begIdx, &v.sar->nbElement, v.sar->vs );
        if(rc) {break;}

        //
        // WILLR - Williams %R
        //
        rc = TA_WILLR( 0, (length-1), high, low, close,
                    v.willr->param[0],
                    &v.willr->begIdx, &v.willr->nbElement, v.willr->vs );
        if(rc) {break;}

        //
        // ADX - Average Directional Movement Index
        //
        rc = TA_ADX( 0, (length-1), high, low, close,
                    v.adx->param[0],
                    &v.adx->begIdx, &v.adx->nbElement, v.adx->vs );
        if(rc) {break;}

        //
        // ADXR - Average Directional Movement Index Rating
        //
        rc = TA_ADXR( 0, (length-1), high, low, close,
                    v.adxr->param[0],
                    &v.adxr->begIdx, &v.adxr->nbElement, v.adxr->vs );
        if(rc) {break;}

        //
        // PLUS_DI - Plus Directional Indicator
        //
        rc = TA_PLUS_DI( 0, (length-1), high, low, close,
                    v.posDI->param[0],
                    &v.posDI->begIdx, &v.posDI->nbElement, v.posDI->vs );
        if(rc) {break;}

        //
        // MINUS_DI - Minus Directional Indicator
        //
        rc = TA_MINUS_DI( 0, (length-1), high, low, close,
                    v.negDI->param[0],
                    &v.negDI->begIdx, &v.negDI->nbElement, v.negDI->vs );
        if(rc) {break;}

        //
        // AD - Chaikin A/D Line
        //
        rc = TA_AD( 0, (length-1), high, low, close, volume,
                    &v.ad->begIdx, &v.ad->nbElement, v.ad->vs );
        if(rc) {break;}

        //
        // ADOSC - Chaikin A/D Oscillator
        //
        rc = TA_ADOSC( 0, (length-1), high, low, close, volume,
                    v.adosc->param[0],
                    v.adosc->param[1],
                    &v.adosc->begIdx, &v.adosc->nbElement, v.adosc->vs );
        if(rc) {break;}

        //
        // OBV - On Balance Volume
        //
        rc = TA_OBV( 0, (length-1), close, volume,
                    &v.obv->begIdx, &v.obv->nbElement, v.obv->vs );
        if(rc) {break;}

        //
        // 出力
        //
        // number: 00
        disp( caption, 0, (length-1), close,
            9,  v.sma, v.ema, v.wma, v.dema, v.tema, v.trima, v.kama, v.mama, v.t3 );
        // number: 01
        disp( caption, 0, (length-1), close,
            5, v.ema12, v.ema26, v.macd, v.signal, v.hist );
        // number: 02
        disp( caption, 0, (length-1), close,
            8,  v.bbup, v.bbmid, v.bblow, v.rsi, v.roc, v.mom, v.sar, v.willr );
        // number: 03
        disp( caption, 0, (length-1), close,
            4,  v.adx, v.adxr, v.posDI, v.negDI );
        // number: 04
        disp( caption, 0, (length-1), close,
            3,  v.ad, v.adosc, v.obv );
    } while(0);

    TA_Shutdown();

    // new_column_t された全領域の解放
    for( int i=0; i<sizeof(struct columns)/sizeof(column_t*); i++ ) {
        free( columns_pp[i] );
    }
    return rc;
}

//
// エントリポイント
//
int main ( int argc, char** argv )
{
    char* inputFileName = NULL;
    char* thisProgramName = NULL;
    row_caption_t* caption = NULL;
    double* open = NULL;
    double* high = NULL;
    double* low = NULL;
    double* close = NULL;
    double* volume = NULL;
    size_t length;

    //
    // 作業開始前にグローバル変数の errno を初期化する
    //
    errno = EXIT_SUCCESS;

    do {
        if( (thisProgramName = calloc( PATH_MAX+1, sizeof(char) )) == NULL ) {break;}
        if( realpath( argv[0], thisProgramName ) == NULL ) {break;}
        if( argc < 2 ) {
            fprintf( stderr, "Usage: %s [file or '-' standard input].\n\n", thisProgramName );
            break;
        }
        fprintf( stderr, "made by \"%s\" program with \"TA-lib %s\"\n", thisProgramName, TA_GetVersionString() );
        //
        if( (inputFileName = calloc( PATH_MAX+1, sizeof(char) )) == NULL ) {break;}
        FILE* fp = NULL;
        enum {FileInput, StdInput} source = FileInput;
        if( strcmp( argv[1], "-" ) == 0) {
            source = StdInput;
            strcpy( inputFileName, "" );
            fprintf( stderr, "Input from stdin\n" );
            fp = stdin;
        } else {
            source = FileInput;
            if( realpath( argv[1], inputFileName ) == NULL ) {break;}
            fprintf( stderr, "Input from file is \"%s\"\n", inputFileName );
            // テキストモードで開く
            if( (fp = fopen( inputFileName, "rt" )) == NULL ) {break;}
        }

        // 入力
        if( read_file( fp, &caption, &open, &high, &low, &close, &volume, &length ) != EXIT_SUCCESS ) {break;}
        if( source == FileInput && fp ) {fclose(fp);}

        // テクニカル分析
        TA_RetCode rc = calculate( caption, open, high, low, close, volume, length );

        // 結果の報告
        TA_RetCodeInfo info;
        TA_SetRetCodeInfo( rc, &info );
        fprintf( stderr, "%d(%s): %s\n", rc, info.enumStr, info.infoStr );
    } while(0);

    free(caption);
    free(open);
    free(high);
    free(low);
    free(close);
    free(volume);
    free(inputFileName);
    free(thisProgramName);
    if(errno) {perror( "main" );}
    return 0;
}

