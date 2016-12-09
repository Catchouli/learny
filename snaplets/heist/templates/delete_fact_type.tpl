<apply template="base">
  <h1>Delete fact type</h1>

  <apply template="_messages"/>

  <p>Are you *sure* you want to delete the fact type <i style="color: red;"><name/></i>?</p>

  <form method="post" action="/fact_types/remove/${id}">
    <input type="submit" />
  </form>
</apply>
