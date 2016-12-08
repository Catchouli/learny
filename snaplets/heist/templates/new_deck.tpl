<apply template="base">
  <h1>Add a new deck</h1>

  <apply template="_messages"/>

  <form method="post" action="/decks/new">
    <table id="info">
      <tr>
        <td>Name:</td><td><input type="text" name="name" size="20" /></td>
      </tr>
      <tr>
        <td></td>
        <td><input type="submit" /></td>
      </tr>
    </table>
  </form>
</apply>
