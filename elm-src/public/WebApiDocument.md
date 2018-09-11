## POST /api/v1/auth

### POST Parameters:

- code
     - **Values**: *Temporary code with in OAuth flow*
     - **Description**: Exchanging a temporary code for an access token


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
{"token":"API Access Token"}
    ```

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

## PUT /api/v1/publish/zmq/:marketCode

### publish price on the zeroMQ infrastructure.


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

## GET /api/v1/stocks/chart/:marketCode

### Get Chart, suffix is only .svg


### Captures:

- *market code*: NI225, TOPIX, TYO8306 etc...

### GET Parameters:

- tf
     - **Values**: *1h, 1d*
     - **Description**: prices of a time frame.

- w
     - **Values**: *int*
     - **Description**: compose chart width. default is 500

- h
     - **Values**: *int*
     - **Description**: compose chart height. default is 500


### Response:

- Status code 200
- Headers: [("Cache-Conrol","no-store")]

- Supported content types are:

    - `image/svg+xml`

- No response body

## DELETE /api/v1/stocks/history/:marketCode

### Delete prices


### Captures:

- *market code*: NI225, TOPIX, TYO8306 etc...

### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

### DELETE Parameters:

- tf
     - **Values**: *1h, 1d*
     - **Description**: prices of a time frame.


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
[]
    ```

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
[{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726}]
    ```

- Example (`application/json;charset=utf-8`):

    ```javascript
[{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726},{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726}]
    ```

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
[{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726}]
    ```

- Example (`application/json;charset=utf-8`):

    ```javascript
[{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726},{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726}]
    ```

## GET /api/v1/stocks/history/:marketCode

### Select prices


### Captures:

- *market code*: NI225, TOPIX, TYO8306 etc...

### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

### GET Parameters:

- tf
     - **Values**: *1h, 1d*
     - **Description**: prices of a time frame.

- limit
     - **Values**: *100, 1000, ...*
     - **Description**: limit of records


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

### Update / Insert prices


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

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
[]
    ```

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
[{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726}]
    ```

- Example (`application/json;charset=utf-8`):

    ```javascript
[{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726},{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726}]
    ```

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
[{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726}]
    ```

- Example (`application/json;charset=utf-8`):

    ```javascript
[{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726},{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726}]
    ```

## PUT /api/v1/stocks/history/:marketCode

### Insert prices


### Captures:

- *market code*: NI225, TOPIX, TYO8306 etc...

### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

### PUT Parameters:

- tf
     - **Values**: *1h, 1d*
     - **Description**: prices of a time frame.


### Request:

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
[{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726}]
    ```

- Example (`application/json;charset=utf-8`):

    ```javascript
[{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726},{"low":715,"at":"2018-03-07T15:00:00+0900","volume":78487400,"close":715,"open":723,"source":"This is example.","high":726}]
    ```

## POST /api/v1/stocks/history/all

### This endpoint runs the crawler.


### Response:

- Status code 202
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
{"buildOS":"linux","gitStatus":"Dirty","gitCommitDate":"Thu Jun 28 19:12:47 2018 +0900","version":"0.4.9","gitCommitCount":"133","gitHash":"9c411f808b67e0bf71219aa771e66178699727ed","gitBranch":"master","buildArch":"x86-64"}
    ```

