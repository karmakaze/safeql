## Why was SafeQL created?

SafeQL arose from the dissatisfaction in the limitations of existing ORM frameworks and query builder libraries. Often what works well in the beginning falls apart at scale. Painful examples of this are non-obvious N+1 queries that creep in and garbage collection pressure that eats memory and kills performance. Avoiding N+1 queries while simultaneously controlling eager/lazy loading is challenging and inefficient at best. Other frameworks are good at making simple things easier. They don't handle complex cases where it would be most beneficial and requires error-prone manual work.

SafeQL is the encapsulation of doing these complex and all-too-common things in a developer friendly and operationally safe way.

## Features

* Composable
* Functional
* Async-enabled
* Batch-load multiple relations
* No N+1 queries
* Type-safe
* Mix expression and SQL
* Protect against SQL injection
* Easy JOINs (foreign-keyed and ad-hoc)
* Define in code or generate from existing schema sources
* Rich (e.g. CTEs)
* Handles IN (&lt;empty&gt;) / NOT IN (&lt;empty&gt;)

## How does SafeQL work?

SafeQL combines several capabilities together to make great the default. Here they are from the highest level to the underlying building blocks:

* functional composition of asynchronous results
* asynchronous batch querying of entities and relationships
* type-safe query composition
* raw queries as typed expressions
* safe SQL statements generated from expressions
* composable templates with smart parameter binding
* code generator from schema sources

## Shut up and show me the code already

* browse [source packages](https://github.com/karmakaze/safeql/tree/master/src/main/java/org/keithkim/safeql)
* browse [test packages](https://github.com/karmakaze/safeql/tree/master/src/test/java/org/keithkim/safeql)

### Querying a single table

```
Accounts accounts = Accounts.where("id >= ?", 1000).get();
  or
Accounts accounts = Accounts.where("id > :min_id", mapOf("min_id", 1000)).get();

Projects projects = accounts.loadProjects("updated_at >= ?", Instant.now().minusDays(30)).get();
```
The `Accounts` and `Projects` types are subclasses of List&lt;Account&gt; and List&lt;Project&gt; with additional methods.

The `projects` returned value of `loadProjects` can be discarded as each account in `accounts` aleady has its projects associated upon loading.

What with all the `get()` calls? Queries return async futures which can be functionally chained. Calling `get()` or `join()` synchronously waits for the result.

### Functional composition

Let's say we want to load some Accounts with some of their Projects and all Project members as well as all active Account admins.

```
CompletableFuture<Accounts> asyncAccounts = Accounts.where("id >= ?", 1000);

CompletableFuture<Member> asyncProjectMembers = Async.pipeline(asyncAccounts,
                              (accounts) -> accounts.loadProjects("updated_at >= ?", Instant.now().minusDays(30)),
                              (projects) -> projects.loadMembers());

CompletableFuture<Admin> asyncActiveAdmins = Async.pipeline(asyncAccounts,
                              (accounts) -> accounts.loadAdmins("active"));

Async.join(asyncProjectMembers, asyncActiveAdmins); // we don't need to wait for projects since loadMembers already does that
```

Now we synchronously have the accounts, admins, recent projects and their members all related in their respective associated collections. We can continue processing from here, or alternatively we could have continued to mix asynchronous processing logic:

```
Map<Project, Admin> notify = Async.combine(asyncProjects, asyncActiveAdmins,
                                 (projects, activeAdmins) -> {
                                     Map<Project, Admin> notify = new HashMap<>();
                                     // some logic here to select key projects and one admin
                                     // notify.put(project, admin);
                                     return notify;
                                 });
```

The pure functional elements can be extracted into testable classes and methods separately from the IO performed between them.
The queries can also be independently tested with known data fixtures.

## Development status
```
Done
[👍] named the project
[👍] no generated N+1 queries
[👍] SQL injection protection

Core complete (needs test coverage)
[✔] handle literal predicate reductions
[✔] handle IN (:values) with empty values condition
[✔] async functional composition
[✔] base expressions
[✔] SQL schema
[✔] mix typed-expression and raw SQL
[✔] batch querying of entities' relations and sub-relations
[✔] SQL predicates
[✔] SQL expressions
[✔] composable base expressions and smart binding
[✔] rich SQL statement features (WITH CTE)
[✔] handle bind of :placeholder with Expr<?>

Working design (explore edge cases)
[⚙] SQL statements

In-progress
[⚠] local bind parameters on constructors
[⚠] composable query expressions and smart binding

Not started
[ ] string literals with embedded spaces in terms
[ ] statement transformation SELECT columns -> COUNT(a.*), COUNT(DISTINCT), EXISTS
[ ] aggregate functions (GROUP BY, HAVING)
[ ] rich SQL statement features (DISTINCT, DISTINCT ON, WINDOW, etc)
[ ] code generator
```

## How to contribute

* Write blog posts
* Comment on existing Github issues
* Submit a Github issue
* Write tests
* Fork the repo on GitHub, submit a PR
