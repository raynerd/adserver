<apply template="base">
  <h1>Testing UI</h1>
  <div>
    <p>Available channel ids (mandatory): <availableChannels/></p>
    <p>Available ad ids: <availableAdIds/></p>
    <p>Available countries: <availableCountries/></p>
    <p>Available languages: <availableLanguages/></p>
    <p>Available interests: <availableInterests/></p>
    <span/>
    <p>
      Channel is mandatory if you don't provide one you will get an error.<br>
      When a valid ad id is requested all remaining parameters ar ignored <br>
      and the specified id is returned. When no specific ad id is filled <br>
      we look for ads for the given country and language. Then we have <br>
      to select by interests, an ad can be candidate if it match interest with <br>
      both channel interests or request interests. If more than one ad match <br>
      the criteria we pick one randomly.
    </p>
  </div>
  <div>
    <table>
      <tr>
        <td>Channel Id:</td>
        <td><input type="number" id="channel-id" size="10"></td>
      </tr>
      <tr>
        <td>Ad Id:</td>
        <td><input type="number" id="ad-id" size="10"></td>
      </tr>
      <tr>
        <td>Country:</td>
        <td><input type="text" id="country" size="10"></td>
      </tr>
      <tr>
        <td>Language:</td>
        <td><input type="text" id="language" size="10"></td>
      </tr>
      <tr>
        <td>Interests (comma separated):</td>
        <td><textarea id="interests" cols="40" rows="5"></textarea></td>
      </tr>
    </table>
  </div>
  <br>
  <div>
    <input id="update-url-btn" type="button" value="Update URL"/>
    <input id="url-input" type="text" size="100"/>
    <input id="request-btn" type="button" value="Request"/>
  </div>
  <div>
    <table>
      <tr>
        <td>Ad Id:</td>
        <td><div id="rsp-id"></div></td>
      </tr>
      <tr>
        <td>Ad Interest:</td>
        <td><div id="rsp-interest"></div></td>
      </tr>
      <tr>
        <td>Ad Start Date:</td>
        <td><div id="rsp-start-date"></div></td>
      </tr>
      <tr>
        <td>Ad End Date:</td>
        <td><div id="rsp-end-date"></div></td>
      </tr>
      <tr>
        <td>Ad Content:</td>
        <td><div id="rsp-content"></div></td>
      </tr>
      <tr>
        <td>Ad Max Views:</td>
        <td><div id="rsp-max-views"></div></td>
      </tr>
      <tr>
        <td>Ad Country:</td>
        <td><div id="rsp-country"></div></td>
      </tr>
      <tr>
        <td>Ad Language:</td>
        <td><div id="rsp-language"></div></td>
      </tr>
    </table>
  </div>
  <bind tag="pageScripts">
    <script src="/static/js/testing.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.8.3/underscore-min.js"></script>
  </bind>
</apply>
