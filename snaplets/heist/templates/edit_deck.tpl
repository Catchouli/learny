<apply template="base">
  <h1>Edit deck</h1>

  <apply template="_messages"/>

  <form method="post" action="/decks/edit/${id}">
    <table id="info">
      <tr>
        <td>Name:</td><td><input type="text" name="name" size="20" value="${name}" /></td>
      </tr>
      <tr>
        <td></td>
        <td><input type="submit" /></td>
      </tr>
    </table>
  </form>
</apply>
