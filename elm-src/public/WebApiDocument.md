## GET /

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/html;charset=utf-8`

- Response body as below.

    ```html
<!DOCTYPE HTML><html lang="ja"><head><meta charset="utf-8"><meta content="width=device-width, initial-scale=1" name="viewport"><title>Dashboard &#8212; TRACTOR</title><link href="https://fonts.googleapis.com/css?family=Roboto:400,300,500|Roboto+Mono|Roboto+Condensed:400,700&amp;subset=latin,latin-ext" type="text/css" rel="stylesheet"><link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet"><link href="https://code.getmdl.io/1.3.0/material.blue_grey-lime.min.css" rel="stylesheet"><link href="https://fonts.googleapis.com/css?family=Gugi" rel="stylesheet"><script src="https://cdnjs.cloudflare.com/ajax/libs/dialog-polyfill/0.4.4/dialog-polyfill.min.js"></script><link href="https://cdnjs.cloudflare.com/ajax/libs/dialog-polyfill/0.4.4/dialog-polyfill.min.css" type="text/css" rel="stylesheet"><script src="https://cdn.polyfill.io/v2/polyfill.js?features=Event.focusin"></script></head><body><script src="public/main.js"></script><script>app = Elm.Main.fullscreen();</script></body></html>
    ```

## GET /api/v1/health

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## GET /api/v1/portfolios

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
[]
    ```

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
[{"caption":"三菱ＵＦＪフィナンシャル・グループ","updateAt":null,"code":"TYO8306"}]
    ```

- Example (`application/json;charset=utf-8`):

    ```javascript
[{"caption":"三菱ＵＦＪフィナンシャル・グループ","updateAt":null,"code":"TYO8306"},{"caption":"三菱ＵＦＪフィナンシャル・グループ","updateAt":null,"code":"TYO8306"}]
    ```

## PUT /api/v1/publish/zmq/:code

### Captures:

- *market code*: NI225, TOPIX, TYO8306 etc...

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript

    ```

## GET /api/v1/quotes/:code

### Captures:

- *market code*: NI225, TOPIX, TYO8306 etc...

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `text/csv;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
[]
    ```

- Example (`text/csv;charset=utf-8`):

    ```
at,code,open,high,low,close,volume,source

    ```

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
[{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"code":"TYO8306","source":"This is example.","high":726}]
    ```

## POST /api/v1/quotes/all/update

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript

    ```

## GET /api/v1/version

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
{"gitStatus":"Dirty","gitCommitDate":"Mon May 28 23:08:08 2018 +0900","version":"0.4.5","gitCommitCount":"124","gitHash":"ebc6549ea89edaed9d918b2e0c848e8f9a22d47d","gitBranch":"master"}
    ```

## GET /public

### Response:

- Status code 200
- Headers: []

- No response body

