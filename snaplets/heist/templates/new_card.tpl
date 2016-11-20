<apply template="base">
  <h1>Add a new card</h1>

  <p><newCardError/></p>

  <bind tag="postAction">/cards/new</bind>
  <bind tag="submitText">Login</bind>

  <form method="post" action="/cards/new">
    <table id="info">
      <tr>
        <td>Front:</td><td><input type="text" name="front" size="20" /></td>
      </tr>
      <tr>
        <td>Back:</td><td><input type="text" name="back" size="20" /></td>
      </tr>
      <tr>
        <td></td>
        <td><input type="submit" /></td>
      </tr>
    </table>
  </form>
</apply>
