<nav class="navbar navbar-inverse navbar-fixed-top">
  <div class="container">
    <div class="navbar-header">
      <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
        <span class="sr-only">Toggle navigation</span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
      </button>
      <a class="navbar-brand" href="/">Learny</a>
    </div>
    <div id="navbar" class="collapse navbar-collapse">
      <ul class="nav navbar-nav">
        <li><a href="/">Home</a></li>
        <ifLoggedIn>
          <li class="dropdown">
            <a href="" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">Review <span class="caret"></span></a>
            <ul class="dropdown-menu">
              <li><a href="/review">Review</a></li>
              <li>&nbsp;&nbsp;Facts</li>
              <li><a href="/facts/new">New</a></li>
              <li><a href="/facts/list">Browse</a></li>
              <li>&nbsp;&nbsp;Decks</li>
              <li><a href="/decks/new">New</a></li>
              <li><a href="/decks/list">List</a></li>
              <li>&nbsp;&nbsp;Fact types</li>
              <li><a href="/fact_types/new">New</a></li>
              <li><a href="/fact_types/list">List</a></li>
            </ul>
          </li>
        </ifLoggedIn>
        <li class="dropdown">
          <a href="" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">Account <span class="caret"></span></a>
          <ul class="dropdown-menu">
            <ifLoggedOut>
              <li><a href="/login">Log in</a></li>
              <li><a href="/new_user">Sign up</a></li>
            </ifLoggedOut>
            <ifLoggedIn>
              <li><a href="/logout">Log out</a></li>
            </ifLoggedIn>
          </ul>
        </li>
      </ul>
    </div><!--/.nav-collapse -->
  </div>
</nav>
