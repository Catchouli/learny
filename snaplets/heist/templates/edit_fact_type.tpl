<apply template="base">
  <h1>Edit fact type</h1>

  <style>
    .inline {
      display: inline;
    }
    
    .link-button {
      background: none;
      border: none;
      color: blue;
      text-decoration: underline;
      cursor: pointer;
      font-size: 1em;
    }
    .link-button:focus {
      outline: none;
    }
    .link-button:active {
      color:red;
    }
  </style>

  <apply template="_messages"/>

  <form method="post" action="/fact_types/edit/${id}">
    <ul>
      <li>
        Name:
        <input type="text" name="name" size="20" value="${name}" />
      </li>
      <li>Fields:</li>
      <li>
        <ul>
          <fields>
            <li>
              <input type="text" name="field-name-${field_id}" value="${name}" />
              <a href="/fact_types/${id}/fields/remove/${field_id}">Remove</a>
            </li>
          </fields>
        </ul>
      </li>
      <li>Card types:</li>
      <li>
        <ul>
          <card_types>
            <li>
              <input type="text" name="card-type-name-${card_type_id}" value="${name}" />
              <a href="/fact_types/${id}/card_types/remove/${card_type_id}">Remove</a>
            </li>
          </card_types>
        </ul>
      </li>
      <li><input type="submit" /></li>
    </ul>
  </form>
  <form method="post" action="/fact_types/${id}/fields/new" class="inline">
    <button type="submit" class="link-button">
      New field
    </button>
  </form>
  <br>
  <form method="post" action="/fact_types/${id}/card_types/new" class="inline">
    <button type="submit" class="link-button">
      New card type
    </button>
  </form>
</apply>
