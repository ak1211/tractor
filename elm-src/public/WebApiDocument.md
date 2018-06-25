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

## GET /api/v1/exchange/temporary/code/:tempCode

### Captures:

- *OAuth temporary code*: Exchanging a temporary code for an access token

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
{"accessToken":"xoxp-????????????-????????????-????????????-????????????????????????????????","userName":"John Doe","scope":"identify.basic"}
    ```

## GET /api/v1/portfolios

### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

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

## PUT /api/v1/publish/zmq/:marketCode

### Captures:

- *market code*: NI225, TOPIX, TYO8306 etc...

### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript

    ```

## POST /api/v1/stocks/history

### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript

    ```

## GET /api/v1/stocks/history/:marketCode

### Captures:

- *market code*: NI225, TOPIX, TYO8306 etc...

### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

### GET Parameters:

- tf
     - **Values**: *1h, 1d*
     - **Description**: prices of a time frame.


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
at,open,high,low,close,volume,source

    ```

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
[{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726}]
    ```

## PATCH /api/v1/stocks/history/:marketCode

### Captures:

- *market code*: NI225, TOPIX, TYO8306 etc...

### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

### PATCH Parameters:

- tf
     - **Values**: *1h, 1d*
     - **Description**: prices of a time frame.


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/x-www-form-urlencoded`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726}
    ```

- Example (`application/x-www-form-urlencoded`):

    ```
low=715.0&at=2018-03-07T15%3A00%3A00%2B0900&volume=78487400&close=715.0&open=723.0&source=This%20is%20example.&high=726.0
    ```

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
{"gitStatus":"Dirty","gitCommitDate":"Sat Jun 23 22:05:26 2018 +0900","version":"0.4.8","gitCommitCount":"128","gitHash":"b638e8a48def56dc7bfb6f729536c547f1d5884b","gitBranch":"master"}
    ```

## GET /public

### Response:

- Status code 200
- Headers: []

- No response body

