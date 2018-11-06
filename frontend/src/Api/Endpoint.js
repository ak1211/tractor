
var putApiV1PublishZmqByMarketCode = function(marketCode, headerAuthorization)
{
  return axios({ url: '/api/v1/publish/zmq/' + encodeURIComponent(marketCode) + ''
    , method: 'put'
    , headers: { "Authorization": headerAuthorization }
    });
}



var postApiV1StocksHistoryAll = function()
{
  return axios({ url: '/api/v1/stocks/history/all'
    , method: 'post'
    });
}



var getApiV1StocksHistoryByMarketCode = function(marketCode, tf, limit, headerAuthorization)
{
  return axios({ url: '/api/v1/stocks/history/' + encodeURIComponent(marketCode) + '' + '?tf=' + encodeURIComponent(tf) + '&limit=' + encodeURIComponent(limit)
    , method: 'get'
    , headers: { "Authorization": headerAuthorization }
    });
}



var putApiV1StocksHistoryByMarketCode = function(marketCode, tf, body, headerAuthorization)
{
  return axios({ url: '/api/v1/stocks/history/' + encodeURIComponent(marketCode) + '' + '?tf=' + encodeURIComponent(tf)
    , method: 'put'
    , data: body
    , responseType: 'json'
    , headers: { "Authorization": headerAuthorization }
    });
}



var patchApiV1StocksHistoryByMarketCode = function(marketCode, tf, body, headerAuthorization)
{
  return axios({ url: '/api/v1/stocks/history/' + encodeURIComponent(marketCode) + '' + '?tf=' + encodeURIComponent(tf)
    , method: 'patch'
    , data: body
    , responseType: 'json'
    , headers: { "Authorization": headerAuthorization }
    });
}



var deleteApiV1StocksHistoryByMarketCode = function(marketCode, tf, body, headerAuthorization)
{
  return axios({ url: '/api/v1/stocks/history/' + encodeURIComponent(marketCode) + '' + '?tf=' + encodeURIComponent(tf)
    , method: 'delete'
    , data: body
    , responseType: 'json'
    , headers: { "Authorization": headerAuthorization }
    });
}



var getApiV1Token = function(headerAuthorization)
{
  return axios({ url: '/api/v1/token'
    , method: 'get'
    , headers: { "Authorization": headerAuthorization }
    });
}



var getApiV1AuthClientid = function()
{
  return axios({ url: '/api/v1/auth/clientid'
    , method: 'get'
    });
}



var getApiV1Version = function()
{
  return axios({ url: '/api/v1/version'
    , method: 'get'
    });
}



var getApiV1Health = function()
{
  return axios({ url: '/api/v1/health'
    , method: 'get'
    });
}



var getApiV1Portfolios = function()
{
  return axios({ url: '/api/v1/portfolios'
    , method: 'get'
    });
}
