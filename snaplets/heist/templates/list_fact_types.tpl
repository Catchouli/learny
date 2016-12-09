<apply template="base">
  <h1>Fact types</h1>

  <apply template="_messages"/>

  <a href="/fact_types/new">Create new fact type</a>

  <ul>
    <factTypes>
      <li>
        <name/>
        (<a href="/fact_types/edit/${id}">edit</a>)
        (<a href="/fact_types/remove/${id}">delete</a>)
      </li>
      <hr>
    </factTypes>
  </ul>
</apply>
