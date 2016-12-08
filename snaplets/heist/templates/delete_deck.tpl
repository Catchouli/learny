<apply template="base">
  <h1>Delete deck</h1>

  <apply template="_messages"/>

  <p>Are you *sure* you want to delete the deck <i style="color: red;"><name/></i>?</p>

  <form method="post" action="/decks/remove/${id}">
    <input type="submit" />
  </form>
</apply>
