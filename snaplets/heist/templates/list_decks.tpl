<apply template="base">
  <h1>Decks</h1>

  <apply template="_messages"/>

  <a href="/decks/new">Add new deck</a>

  <ul>
    <decks>
      <li>
        <name/>
        (<a href="/decks/edit/${id}">edit</a>)
        (<a href="/decks/remove/${id}">delete</a>)
      </li>
      <hr>
    </decks>
  </ul>
</apply>
