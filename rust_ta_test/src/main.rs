///
/// TA-libでテクニカル指標を計算するプログラム
/// https://ak1211.com
/// Copyright (c) 2018 Akihiro Yamamoto
///
/// This software is released under the MIT License.
/// http://opensource.org/licenses/mit-license.php
///
extern crate ta_lib_wrapper;
use std::cmp;
use std::env;
use std::ffi;
use std::fmt;
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::mem;
use std::str::FromStr;
use ta_lib_wrapper::{TA_FuncUnstId, TA_GetVersionString, TA_Initialize, TA_Integer, TA_MAType,
                     TA_Real, TA_RetCode, TA_RetCodeInfo, TA_SetRetCodeInfo, TA_SetUnstablePeriod,
                     TA_Shutdown, TA_AD, TA_ADOSC, TA_ADX, TA_ADXR, TA_BBANDS, TA_MA, TA_MACD,
                     TA_MINUS_DI, TA_MOM, TA_OBV, TA_PLUS_DI, TA_ROC, TA_RSI, TA_SAR, TA_WILLR};

// 日付時刻列の幅
const DATE_TIME_COLUMN_WIDTH: usize = 24;
// 列の最小幅
const COLUMN_WIDTH_MIN: usize = 9;

///
/// 日本時間
///
#[derive(Clone)]
struct AsiaTokyoDateTime {
    date: String,
    time: String,
}

impl fmt::Display for AsiaTokyoDateTime {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // timezone : Asia / Tokyo
        write!(f, "{}T{}+0900", self.date, self.time)
    }
}

impl AsiaTokyoDateTime {
    fn new(d: &str, t: &str) -> AsiaTokyoDateTime {
        AsiaTokyoDateTime {
            date: d.to_string(),
            time: t.to_string(),
        }
    }
}

///
/// 初値
///
struct Open<T>(T);

///
/// 高値
///
struct High<T>(T);

///
/// 安値
///
struct Low<T>(T);

///
/// 終値
///
struct Close<T>(T);

///
/// 出来高
///
struct Volume<T>(T);

///
/// 行の値と構造
///
struct Row<T> {
    date_time: AsiaTokyoDateTime,
    open: Open<T>,
    high: High<T>,
    low: Low<T>,
    close: Close<T>,
    volume: Volume<T>,
}

///
/// 入力から行を読む
///
fn read_file<T: FromStr>(stream: &mut BufRead) -> Vec<Row<T>> {
    let mut built = Vec::new();
    let mut line = String::new();
    let mut line_number: u64 = 1;

    // 先頭２行は読み捨てる
    let _ = stream.read_line(&mut line);
    let _ = stream.read_line(&mut line);
    line.clear();
    //
    while stream.read_line(&mut line).unwrap() > 0 {
        // 空行は無視する
        if !line.trim().is_empty() {
            built.push(parse_line(line_number, &line).unwrap());
        }
        line.clear();
        line_number = line_number + 1;
    }
    return built;
}

///
/// タブ区切りの行を分解して、値と構造を得る
///
fn parse_line<T: FromStr>(line_number: u64, line: &str) -> Result<Row<T>, String> {
    let errmsg = |s: &str| format!("line {}, No parse of '{}'.", line_number, s);
    let parts: Vec<&str> = line.split('\t').map(|a| a.trim()).collect();
    match parts.as_slice() {
        [d, t, o, h, l, c, v] => {
            let date_time = AsiaTokyoDateTime::new(d, t);
            let oo = try!(o.parse().map_err(|_| errmsg(o)));
            let hh = try!(h.parse().map_err(|_| errmsg(h)));
            let ll = try!(l.parse().map_err(|_| errmsg(l)));
            let cc = try!(c.parse().map_err(|_| errmsg(c)));
            let vv = try!(v.parse().map_err(|_| errmsg(v)));
            return Ok(Row {
                date_time: date_time,
                open: Open(oo),
                high: High(hh),
                low: Low(ll),
                close: Close(cc),
                volume: Volume(vv),
            });
        }
        _ => return Err(format!("line {}, Column mismatch.", line_number)),
    }
}

///
/// テクニカル指標のオプション
///
struct MacdPeriods {
    fast: u32,
    slow: u32,
    signal: u32,
}

struct BbandsOpt {
    period: u32,
    dev_up: TA_Real,
    dev_down: TA_Real,
}

struct SarOpt {
    acceleration: TA_Real,
    maximum: TA_Real,
}

struct AdoscPeriods {
    fast: u32,
    slow: u32,
}

///
/// テクニカル指標
///
enum Indicator {
    Dema(u32),
    Ema(u32),
    Kama(u32),
    Mama(u32),
    Sma(u32),
    T3(u32),
    Tema(u32),
    Trima(u32),
    Wma(u32),
    Macd(MacdPeriods),
    Bbands(BbandsOpt),
    Rsi(u32),
    Roc(u32),
    Mom(u32),
    Sar(SarOpt),
    WillR(u32),
    Adx(u32),
    AdxR(u32),
    PlusDI(u32),
    MinusDI(u32),
    Ad,
    Adosc(AdoscPeriods),
    Obv,
}

///
/// 列
///
struct Column {
    caption: String,
    begin_idx: usize,
    values: Vec<TA_Real>,
}

impl Column {
    fn new(name: &str, opt: Option<&str>, begin_idx: usize, values: Vec<TA_Real>) -> Column {
        Column {
            caption: name.to_string() + opt.unwrap_or(""),
            begin_idx: begin_idx,
            values: values,
        }
    }
    ///
    /// 列見出しを出す
    ///
    fn show_caption(&self) -> String {
        format!("{:>width$}", self.caption, width = self.width())
    }
    ///
    /// ix行の値を出す
    ///
    fn show_nth(&self, ix: usize) -> String {
        let offset = ix as isize - self.begin_idx as isize;
        if offset < 0 {
            return format!("{:>width$}", "N/A", width = self.width());
        } else {
            return format!(
                "{:width$.2}",
                self.values[offset as usize],
                width = self.width()
            );
        }
    }
    ///
    /// 列の幅
    ///
    fn width(&self) -> usize {
        return cmp::max(self.caption.len(), COLUMN_WIDTH_MIN);
    }
}

///
/// 単なる入力行の転置行列
///
#[allow(dead_code)]
struct Prices {
    date_time: Vec<AsiaTokyoDateTime>,
    open: Open<Vec<TA_Real>>,
    high: High<Vec<TA_Real>>,
    low: Low<Vec<TA_Real>>,
    close: Close<Vec<TA_Real>>,
    volume: Volume<Vec<TA_Real>>,
}

impl Prices {
    ///
    /// 行を転置して格納する
    ///
    fn new(rows: &Vec<Row<TA_Real>>) -> Prices {
        Prices {
            date_time: rows.iter().map(|a| a.date_time.clone()).collect(),
            open: Open(rows.iter().map(|a| a.open.0).collect()),
            high: High(rows.iter().map(|a| a.high.0).collect()),
            low: Low(rows.iter().map(|a| a.low.0).collect()),
            close: Close(rows.iter().map(|a| a.close.0).collect()),
            volume: Volume(rows.iter().map(|a| a.volume.0).collect()),
        }
    }
    ///
    /// 値を陳列して出力する
    ///
    fn disp(&self, cols: Vec<&Column>) -> io::Result<()> {
        // 出力に出す入力の値
        let close: Column = {
            let Close(ref close_prices) = self.close;
            Column::new("Close", None, 0, close_prices.clone())
        };
        // 出力に出す入力の値を含んだ出力値の列
        let columns: Vec<&Column> = vec![vec![&close], cols]
            .into_iter()
            .flat_map(|a| a)
            .collect();
        // 行バッファ(サイズは適当)
        let mut buff = String::with_capacity(1024);
        //
        // 行バッファに見だしを蓄積していく
        //
        buff.push_str(&format!(
            "{:>width$}",
            "DateTime",
            width = DATE_TIME_COLUMN_WIDTH
        ));
        // それぞれの見だしを','区切りで結合する
        for v in &columns {
            buff.push(',');
            buff.push_str(&v.show_caption());
        }
        buff.push('\n');
        //
        // 各行を出力する
        //
        let mut writer = BufWriter::new(io::stdout());
        for (ix, date_time) in self.date_time.iter().enumerate() {
            // 日付
            buff.push_str(&format!("{}", date_time));
            // それぞれの値を','区切りで結合する
            for v in &columns {
                buff.push(',');
                buff.push_str(&v.show_nth(ix));
            }
            buff.push('\n');
            // 出力する
            writer.write(buff.as_bytes())?;
            buff.clear();
        }
        writer.flush()
    }
    ///
    /// 指定のテクニカル指標を計算する
    ///
    fn calculate(&self, targets: &[Indicator]) -> Result<Vec<Column>, String> {
        // TA-libの初期化
        unsafe {
            match TA_Initialize() {
                TA_RetCode::TA_SUCCESS => (),
                a => panic!("fail to TA_Initialize, err: {:?}", a),
            }
            // EMAが安定するまでの数
            match TA_SetUnstablePeriod(TA_FuncUnstId::TA_FUNC_UNST_EMA, 20) {
                TA_RetCode::TA_SUCCESS => (),
                a => panic!("fail to TA_SetUnstablePeriod, err: {:?}", a),
            }
        }
        // それぞれのテクニカル指標を計算する
        let built = targets
            .iter()
            .map(|a| self.calc_singlar(a))
            .collect::<Result<Vec<Vec<Column>>, TA_RetCode>>();
        // TA-libの終了
        unsafe {
            TA_Shutdown();
        }
        // テクニカル指標の計算結果を返す
        match built {
            Ok(a) => return Ok(a.into_iter().flat_map(|v| v).collect::<Vec<Column>>()),
            Err(rc) => {
                //
                // 失敗したらTA-libから失敗原因を得て返却
                //
                let (enm, inf): (&ffi::CStr, &ffi::CStr) = unsafe {
                    let mut rcinfo: TA_RetCodeInfo = mem::zeroed();
                    TA_SetRetCodeInfo(rc, &mut rcinfo);
                    (
                        ffi::CStr::from_ptr(rcinfo.enumStr),
                        ffi::CStr::from_ptr(rcinfo.infoStr),
                    )
                };
                return Err(format!(
                    "{}:{}",
                    enm.to_str().unwrap(),
                    inf.to_str().unwrap()
                ));
            }
        }
    }
    ///
    /// テクニカル指標を計算する(内部用)
    ///
    fn calc_singlar(&self, target: &Indicator) -> Result<Vec<Column>, TA_RetCode> {
        match target {
            Indicator::Dema(op) => self.calc_ma("DEMA", TA_MAType::TA_MAType_DEMA, *op),
            Indicator::Ema(op) => self.calc_ma("EMA", TA_MAType::TA_MAType_EMA, *op),
            Indicator::Kama(op) => self.calc_ma("KAMA", TA_MAType::TA_MAType_KAMA, *op),
            Indicator::Mama(op) => self.calc_ma("MAMA", TA_MAType::TA_MAType_MAMA, *op),
            Indicator::Sma(op) => self.calc_ma("SMA", TA_MAType::TA_MAType_SMA, *op),
            Indicator::T3(op) => self.calc_ma("T3", TA_MAType::TA_MAType_T3, *op),
            Indicator::Tema(op) => self.calc_ma("TEMA", TA_MAType::TA_MAType_TEMA, *op),
            Indicator::Trima(op) => self.calc_ma("TRIMA", TA_MAType::TA_MAType_TRIMA, *op),
            Indicator::Wma(op) => self.calc_ma("WMA", TA_MAType::TA_MAType_WMA, *op),
            Indicator::Macd(op) => self.calc_macd(op),
            Indicator::Bbands(op) => self.calc_bbands(op),
            Indicator::Rsi(op) => self.calc_rsi(*op),
            Indicator::Roc(op) => self.calc_roc(*op),
            Indicator::Mom(op) => self.calc_mom(*op),
            Indicator::Sar(op) => self.calc_sar(op),
            Indicator::WillR(op) => self.calc_willr(*op),
            Indicator::Adx(op) => self.calc_adx(*op),
            Indicator::AdxR(op) => self.calc_adxr(*op),
            Indicator::PlusDI(op) => self.calc_plus_di(*op),
            Indicator::MinusDI(op) => self.calc_minus_di(*op),
            Indicator::Ad => self.calc_ad(),
            Indicator::Adosc(op) => self.calc_adosc(op),
            Indicator::Obv => self.calc_obv(),
        }
    }
    ///
    /// Moving Average
    ///
    fn calc_ma(&self, name: &str, typ: TA_MAType, period: u32) -> Result<Vec<Column>, TA_RetCode> {
        let Close(ref close_prices) = self.close;
        let mut out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut out_begin: TA_Integer = 0;
        let mut out_size: TA_Integer = 0;

        unsafe {
            let ret_code = TA_MA(
                0,
                close_prices.len() as i32 - 1,
                close_prices.as_ptr(),
                period as i32,
                typ,
                &mut out_begin,
                &mut out_size,
                out.as_mut_ptr(),
            );
            if ret_code == TA_RetCode::TA_SUCCESS {
                out.set_len(out_size as usize);
            } else {
                return Err(ret_code);
            }
        }
        return Ok(vec![
            Column::new(
                name,
                Some(&format!("({:.2})", period)),
                out_begin as usize,
                out,
            ),
        ]);
    }
    ///
    /// MACD - Moving Average Convergence/Divergence
    ///
    fn calc_macd(&self, period: &MacdPeriods) -> Result<Vec<Column>, TA_RetCode> {
        let Close(ref close_prices) = self.close;
        let mut macd_out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut signal_out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut histogram_out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut out_begin: TA_Integer = 0;
        let mut out_size: TA_Integer = 0;

        unsafe {
            let ret_code = TA_MACD(
                0,
                close_prices.len() as i32 - 1,
                close_prices.as_ptr(),
                period.fast as i32,
                period.slow as i32,
                period.signal as i32,
                &mut out_begin,
                &mut out_size,
                macd_out.as_mut_ptr(),
                signal_out.as_mut_ptr(),
                histogram_out.as_mut_ptr(),
            );
            if ret_code == TA_RetCode::TA_SUCCESS {
                macd_out.set_len(out_size as usize);
                signal_out.set_len(out_size as usize);
                histogram_out.set_len(out_size as usize);
            } else {
                return Err(ret_code);
            }
        }
        let s = format!(
            "({:.2},{:.2},{:.2})",
            period.fast, period.slow, period.signal
        );
        return Ok(vec![
            Column::new("MACD", Some(&s), out_begin as usize, macd_out),
            Column::new("Signal", Some(&s), out_begin as usize, signal_out),
            Column::new("Hist", Some(&s), out_begin as usize, histogram_out),
        ]);
    }
    ///
    /// BBANDS - Bollinger Bands
    ///
    fn calc_bbands(&self, opt: &BbandsOpt) -> Result<Vec<Column>, TA_RetCode> {
        let Close(ref close_prices) = self.close;
        let mut up_out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut mid_out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut low_out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut out_begin: TA_Integer = 0;
        let mut out_size: TA_Integer = 0;

        unsafe {
            let ret_code = TA_BBANDS(
                0,
                close_prices.len() as i32 - 1,
                close_prices.as_ptr(),
                opt.period as i32,
                opt.dev_up,
                opt.dev_down,
                TA_MAType::TA_MAType_SMA,
                &mut out_begin,
                &mut out_size,
                up_out.as_mut_ptr(),
                mid_out.as_mut_ptr(),
                low_out.as_mut_ptr(),
            );
            if ret_code == TA_RetCode::TA_SUCCESS {
                up_out.set_len(out_size as usize);
                mid_out.set_len(out_size as usize);
                low_out.set_len(out_size as usize);
            } else {
                return Err(ret_code);
            }
        }
        let s = format!("({:.2},{:.2},{:.2})", opt.period, opt.dev_up, opt.dev_down);
        return Ok(vec![
            Column::new("BBUP", Some(&s), out_begin as usize, up_out),
            Column::new("BBMID", Some(&s), out_begin as usize, mid_out),
            Column::new("BBLOW", Some(&s), out_begin as usize, low_out),
        ]);
    }
    ///
    /// RSI - Relative Strength Index
    ///
    fn calc_rsi(&self, period: u32) -> Result<Vec<Column>, TA_RetCode> {
        let Close(ref close_prices) = self.close;
        let mut out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut out_begin: TA_Integer = 0;
        let mut out_size: TA_Integer = 0;

        unsafe {
            let ret_code = TA_RSI(
                0,
                close_prices.len() as i32 - 1,
                close_prices.as_ptr(),
                period as i32,
                &mut out_begin,
                &mut out_size,
                out.as_mut_ptr(),
            );
            if ret_code == TA_RetCode::TA_SUCCESS {
                out.set_len(out_size as usize);
            } else {
                return Err(ret_code);
            }
        }
        return Ok(vec![
            Column::new(
                "RSI",
                Some(&format!("({:.2})", period)),
                out_begin as usize,
                out,
            ),
        ]);
    }
    ///
    /// ROC - Rate of change : ((price/prevPrice)-1)*100
    ///
    fn calc_roc(&self, period: u32) -> Result<Vec<Column>, TA_RetCode> {
        let Close(ref close_prices) = self.close;
        let mut out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut out_begin: TA_Integer = 0;
        let mut out_size: TA_Integer = 0;

        unsafe {
            let ret_code = TA_ROC(
                0,
                close_prices.len() as i32 - 1,
                close_prices.as_ptr(),
                period as i32,
                &mut out_begin,
                &mut out_size,
                out.as_mut_ptr(),
            );
            if ret_code == TA_RetCode::TA_SUCCESS {
                out.set_len(out_size as usize);
            } else {
                return Err(ret_code);
            }
        }
        return Ok(vec![
            Column::new(
                "ROC",
                Some(&format!("({:.2})", period)),
                out_begin as usize,
                out,
            ),
        ]);
    }
    ///
    /// MOM - Momentum
    ///
    fn calc_mom(&self, period: u32) -> Result<Vec<Column>, TA_RetCode> {
        let Close(ref close_prices) = self.close;
        let mut out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut out_begin: TA_Integer = 0;
        let mut out_size: TA_Integer = 0;

        unsafe {
            let ret_code = TA_MOM(
                0,
                close_prices.len() as i32 - 1,
                close_prices.as_ptr(),
                period as i32,
                &mut out_begin,
                &mut out_size,
                out.as_mut_ptr(),
            );
            if ret_code == TA_RetCode::TA_SUCCESS {
                out.set_len(out_size as usize);
            } else {
                return Err(ret_code);
            }
        }
        return Ok(vec![
            Column::new(
                "MOM",
                Some(&format!("({:.2})", period)),
                out_begin as usize,
                out,
            ),
        ]);
    }
    ///
    /// SAR - Parabolic SAR
    ///
    fn calc_sar(&self, opt: &SarOpt) -> Result<Vec<Column>, TA_RetCode> {
        let High(ref high_prices) = self.high;
        let Low(ref low_prices) = self.low;
        let mut out: Vec<TA_Real> = Vec::with_capacity(high_prices.len());
        let mut out_begin: TA_Integer = 0;
        let mut out_size: TA_Integer = 0;

        unsafe {
            let ret_code = TA_SAR(
                0,
                high_prices.len() as i32 - 1,
                high_prices.as_ptr(),
                low_prices.as_ptr(),
                opt.acceleration,
                opt.maximum,
                &mut out_begin,
                &mut out_size,
                out.as_mut_ptr(),
            );
            if ret_code == TA_RetCode::TA_SUCCESS {
                out.set_len(out_size as usize);
            } else {
                return Err(ret_code);
            }
        }
        let s = format!("({:.2},{:.2})", opt.acceleration, opt.maximum);
        return Ok(vec![Column::new("SAR", Some(&s), out_begin as usize, out)]);
    }
    ///
    /// WILLR - Williams %R
    ///
    fn calc_willr(&self, period: u32) -> Result<Vec<Column>, TA_RetCode> {
        let High(ref high_prices) = self.high;
        let Low(ref low_prices) = self.low;
        let Close(ref close_prices) = self.close;
        let mut out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut out_begin: TA_Integer = 0;
        let mut out_size: TA_Integer = 0;

        unsafe {
            let ret_code = TA_WILLR(
                0,
                close_prices.len() as i32 - 1,
                high_prices.as_ptr(),
                low_prices.as_ptr(),
                close_prices.as_ptr(),
                period as i32,
                &mut out_begin,
                &mut out_size,
                out.as_mut_ptr(),
            );
            if ret_code == TA_RetCode::TA_SUCCESS {
                out.set_len(out_size as usize);
            } else {
                return Err(ret_code);
            }
        }
        return Ok(vec![
            Column::new(
                "Williams%R",
                Some(&format!("({:.2})", period)),
                out_begin as usize,
                out,
            ),
        ]);
    }
    ///
    /// ADX - Average Directional Movement Index
    ///
    fn calc_adx(&self, period: u32) -> Result<Vec<Column>, TA_RetCode> {
        let High(ref high_prices) = self.high;
        let Low(ref low_prices) = self.low;
        let Close(ref close_prices) = self.close;
        let mut out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut out_begin: TA_Integer = 0;
        let mut out_size: TA_Integer = 0;

        unsafe {
            let ret_code = TA_ADX(
                0,
                close_prices.len() as i32 - 1,
                high_prices.as_ptr(),
                low_prices.as_ptr(),
                close_prices.as_ptr(),
                period as i32,
                &mut out_begin,
                &mut out_size,
                out.as_mut_ptr(),
            );
            if ret_code == TA_RetCode::TA_SUCCESS {
                out.set_len(out_size as usize);
            } else {
                return Err(ret_code);
            }
        }
        return Ok(vec![
            Column::new(
                "ADX",
                Some(&format!("({:.2})", period)),
                out_begin as usize,
                out,
            ),
        ]);
    }
    ///
    /// ADXR - Average Directional Movement Index Rating
    ///
    fn calc_adxr(&self, period: u32) -> Result<Vec<Column>, TA_RetCode> {
        let High(ref high_prices) = self.high;
        let Low(ref low_prices) = self.low;
        let Close(ref close_prices) = self.close;
        let mut out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut out_begin: TA_Integer = 0;
        let mut out_size: TA_Integer = 0;

        unsafe {
            let ret_code = TA_ADXR(
                0,
                close_prices.len() as i32 - 1,
                high_prices.as_ptr(),
                low_prices.as_ptr(),
                close_prices.as_ptr(),
                period as i32,
                &mut out_begin,
                &mut out_size,
                out.as_mut_ptr(),
            );
            if ret_code == TA_RetCode::TA_SUCCESS {
                out.set_len(out_size as usize);
            } else {
                return Err(ret_code);
            }
        }
        return Ok(vec![
            Column::new(
                "ADXR",
                Some(&format!("({:.2})", period)),
                out_begin as usize,
                out,
            ),
        ]);
    }
    ///
    /// PLUS_DI - Plus Directional Indicator
    ///
    fn calc_plus_di(&self, period: u32) -> Result<Vec<Column>, TA_RetCode> {
        let High(ref high_prices) = self.high;
        let Low(ref low_prices) = self.low;
        let Close(ref close_prices) = self.close;
        let mut out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut out_begin: TA_Integer = 0;
        let mut out_size: TA_Integer = 0;

        unsafe {
            let ret_code = TA_PLUS_DI(
                0,
                close_prices.len() as i32 - 1,
                high_prices.as_ptr(),
                low_prices.as_ptr(),
                close_prices.as_ptr(),
                period as i32,
                &mut out_begin,
                &mut out_size,
                out.as_mut_ptr(),
            );
            if ret_code == TA_RetCode::TA_SUCCESS {
                out.set_len(out_size as usize);
            } else {
                return Err(ret_code);
            }
        }
        return Ok(vec![
            Column::new(
                "+DI",
                Some(&format!("({:.2})", period)),
                out_begin as usize,
                out,
            ),
        ]);
    }
    ///
    /// MINUS_DI - Minus Directional Indicator
    ///
    fn calc_minus_di(&self, period: u32) -> Result<Vec<Column>, TA_RetCode> {
        let High(ref high_prices) = self.high;
        let Low(ref low_prices) = self.low;
        let Close(ref close_prices) = self.close;
        let mut out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut out_begin: TA_Integer = 0;
        let mut out_size: TA_Integer = 0;

        unsafe {
            let ret_code = TA_MINUS_DI(
                0,
                close_prices.len() as i32 - 1,
                high_prices.as_ptr(),
                low_prices.as_ptr(),
                close_prices.as_ptr(),
                period as i32,
                &mut out_begin,
                &mut out_size,
                out.as_mut_ptr(),
            );
            if ret_code == TA_RetCode::TA_SUCCESS {
                out.set_len(out_size as usize);
            } else {
                return Err(ret_code);
            }
        }
        return Ok(vec![
            Column::new(
                "-DI",
                Some(&format!("({:.2})", period)),
                out_begin as usize,
                out,
            ),
        ]);
    }
    ///
    /// AD - Chaikin A/D Line
    ///
    fn calc_ad(&self) -> Result<Vec<Column>, TA_RetCode> {
        let High(ref high_prices) = self.high;
        let Low(ref low_prices) = self.low;
        let Close(ref close_prices) = self.close;
        let Volume(ref volume) = self.volume;
        let mut out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut out_begin: TA_Integer = 0;
        let mut out_size: TA_Integer = 0;

        unsafe {
            let ret_code = TA_AD(
                0,
                close_prices.len() as i32 - 1,
                high_prices.as_ptr(),
                low_prices.as_ptr(),
                close_prices.as_ptr(),
                volume.as_ptr(),
                &mut out_begin,
                &mut out_size,
                out.as_mut_ptr(),
            );
            if ret_code == TA_RetCode::TA_SUCCESS {
                out.set_len(out_size as usize);
            } else {
                return Err(ret_code);
            }
        }
        return Ok(vec![
            Column::new("Chaikin A/D Line", None, out_begin as usize, out),
        ]);
    }
    ///
    /// ADOSC - Chaikin A/D Oscillator
    ///
    fn calc_adosc(&self, period: &AdoscPeriods) -> Result<Vec<Column>, TA_RetCode> {
        let High(ref high_prices) = self.high;
        let Low(ref low_prices) = self.low;
        let Close(ref close_prices) = self.close;
        let Volume(ref volume) = self.volume;
        let mut out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut out_begin: TA_Integer = 0;
        let mut out_size: TA_Integer = 0;

        unsafe {
            let ret_code = TA_ADOSC(
                0,
                close_prices.len() as i32 - 1,
                high_prices.as_ptr(),
                low_prices.as_ptr(),
                close_prices.as_ptr(),
                volume.as_ptr(),
                period.fast as i32,
                period.slow as i32,
                &mut out_begin,
                &mut out_size,
                out.as_mut_ptr(),
            );
            if ret_code == TA_RetCode::TA_SUCCESS {
                out.set_len(out_size as usize);
            } else {
                return Err(ret_code);
            }
        }
        return Ok(vec![
            Column::new(
                "Chaikin A/D Oscillator",
                Some(&format!("({:.2},{:.2})", period.fast, period.slow)),
                out_begin as usize,
                out,
            ),
        ]);
    }
    ///
    /// OBV - On Balance Volume
    ///
    fn calc_obv(&self) -> Result<Vec<Column>, TA_RetCode> {
        let Close(ref close_prices) = self.close;
        let Volume(ref volume) = self.volume;
        let mut out: Vec<TA_Real> = Vec::with_capacity(close_prices.len());
        let mut out_begin: TA_Integer = 0;
        let mut out_size: TA_Integer = 0;

        unsafe {
            let ret_code = TA_OBV(
                0,
                close_prices.len() as i32 - 1,
                close_prices.as_ptr(),
                volume.as_ptr(),
                &mut out_begin,
                &mut out_size,
                out.as_mut_ptr(),
            );
            if ret_code == TA_RetCode::TA_SUCCESS {
                out.set_len(out_size as usize);
            } else {
                return Err(ret_code);
            }
        }
        return Ok(vec![
            Column::new("On Balance Volume", None, out_begin as usize, out),
        ]);
    }
}

///
/// エントリポイント
///
fn main() {
    let this_program_name = env::args().next().unwrap();
    let input_file_name = match env::args().nth(1) {
        Some(a) => a,
        None => {
            eprintln!("Usage: {} [file or '-' standard input].", this_program_name);
            return;
        }
    };
    let ta_version_string: &ffi::CStr = unsafe { ffi::CStr::from_ptr(TA_GetVersionString()) };
    eprintln!(
        "made by \"{}\" program, with \"TA-lib {}\"",
        this_program_name,
        ta_version_string.to_str().unwrap()
    );
    //
    // 入力を読み込む
    //
    let prices = if input_file_name == "-" {
        let mut reader = BufReader::new(io::stdin());
        Prices::new(&read_file(&mut reader))
    } else {
        let mut reader = BufReader::new(File::open(&input_file_name).unwrap());
        Prices::new(&read_file(&mut reader))
    };
    //
    // 計算する対象のテクニカル指標
    //
    let indicators = [
        // number: 00
        vec![
            Indicator::Sma(12),
            Indicator::Ema(12),
            Indicator::Wma(12),
            Indicator::Dema(12),
            Indicator::Tema(12),
            Indicator::Trima(12),
            Indicator::Kama(12),
            Indicator::Mama(12),
            Indicator::T3(12),
        ],
        // number: 01
        vec![
            Indicator::Ema(12),
            Indicator::Ema(26),
            Indicator::Macd(MacdPeriods {
                fast: 12,
                slow: 26,
                signal: 9,
            }),
        ],
        // number: 02
        vec![
            Indicator::Bbands(BbandsOpt {
                period: 25,
                dev_up: 1.5,
                dev_down: 1.5,
            }),
            Indicator::Rsi(14),
            Indicator::Roc(14),
            Indicator::Mom(14),
            Indicator::Sar(SarOpt {
                acceleration: 0.04,
                maximum: 0.2,
            }),
            Indicator::WillR(14),
        ],
        // number: 03
        vec![
            Indicator::Adx(9),
            Indicator::AdxR(9),
            Indicator::PlusDI(14),
            Indicator::MinusDI(14),
        ],
        // number: 04
        vec![
            Indicator::Ad,
            Indicator::Adosc(AdoscPeriods { fast: 3, slow: 10 }),
            Indicator::Obv,
        ],
    ];
    //
    // テクニカル指標の計算と出力
    //
    for inds in indicators.iter() {
        let columns = prices.calculate(inds).unwrap();
        let _ = prices.disp(columns.iter().collect());
    }
}
