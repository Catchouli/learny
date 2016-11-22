<apply template="base">
  <h1>Edit card</h1>

  <p><error/></p>

  <bind tag="postAction">/cards/edit</bind>
  <bind tag="submitText">Login</bind>

  <form method="post" action="/cards/new">
    <table id="info">
      <tr>
        <td>Front:</td><td><input type="text" name="front" size="20" value="${front}" /></td>
      </tr>
      <tr>
        <td>Back:</td><td><input type="text" name="back" size="20" value="${back}" /></td>
      </tr>
      <tr>
        <td></td>
        <td><input type="submit" /></td>
      </tr>
    </table>
  </form>
</apply>
