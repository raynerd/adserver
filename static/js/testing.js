"use strict";
$(document).ready(function() {
  $('#update-url-btn').click(function() {
    updateUrl();
  });
  $('#request-btn').click(function() {
    getAd();
  });
});

function updateUrl() {
  var channelId = $('#channel-id').prop('value');
  var adId = $('#ad-id').prop('value');
  var country = $('#country').prop('value');
  var language = $('#language').prop('value');
  var interests = $('#interests').prop('value');

  var baseUrl = "/api/";
  var fullUrl = baseUrl;
  fullUrl = fullUrl.concat(channelId.toString());
  fullUrl = fullUrl.concat("?");
  var urlParameters =
    [ getPair("ad-id", adId)
    , getPair("country", country)
    , getPair("language", language)
    ]
  var interestParams = getInterestParams(interests);
  urlParameters = _.union(urlParameters, interestParams);
  var nonEmpty = _.reject(urlParameters, function(parameter) {
        return parameter === "";
  });
  console.log(nonEmpty.join("&"));
  fullUrl = fullUrl.concat(nonEmpty.join("&"))
  $('#url-input').prop('value', fullUrl);
}

function getPair(key, value) {
  if (value) {
    var capitalizeV = value.replace(/\b\w/g, l => l.toUpperCase());
    return key + "=" + capitalizeV;
  }
  return "";
}

function getInterestParams(interests) {
  var arrInterests = interests.split(",");
  return _.uniq(_.map(arrInterests, function(interest) {
    var clInterest = interest;
    return getPair("interest", clInterest.trim());
  }));
}

function getAd() {
  var reqUrl = $('#url-input').prop('value');
  var responseArea = $('#response');
  var responseText = "";
  if(reqUrl !== "") {
    $.ajax( reqUrl, {
           'type': 'GET',
         })
     .done(function(response) {
       renderResponse(response);
      //  responseArea.prop('value', JSON.stringify(response));
     })
     .fail(function(msg) {
       responseArea.prop('value',  "Error: " + msg);
     });
   }
   else {
     responseArea.prop('value', "Warning: No URL set");
   }
}

function renderResponse(response) {
  $('#rsp-id').text(response.adId);
  $('#rsp-interest').text(response.adInterest);
  $('#rsp-start-date').text(response.adStartDate);
  $('#rsp-end-date').text(response.adEndDate);
  $('#rsp-content').text(response.adContent);
  $('#rsp-max-views').text(response.adMaxViews);
  $('#rsp-country').text(response.adCountry.countryName);
  $('#rsp-language').text(response.adLanguage);
}
