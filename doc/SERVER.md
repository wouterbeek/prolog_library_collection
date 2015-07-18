# HTTP default settings in SWI-Prolog


## HTTP locations

SWI-Prolog defines the following HTTP locations:

| *Abbreviation*   | *Path*               |
| =css=            | =|root(css)|=        |
| =icons=          | =|root(icons)|=      |
| =js=             | =|root(js)|=         |
| =pldoc=          | =|root(.)|=          |
| =pldoc_man=      | =|pldoc(refman)|=    |
| =pldoc_pkg=      | =|pldoc(package)|=   |
| =pldoc_resource= | =|/debug/help/res/|= |
| =root=           | =|/|=                |


## HTTP handlers

SWI-Prolog defines the following HTTP handlers:

| *Spec*                  | *Handler*                                             |
| =|css(.)|=              | =|http_server_files:serve_files_in_directory(css)|=   |
| =|icons(.)|=            | =|http_server_files:serve_files_in_directory(icons)|= |
| =|js(.)|=               | =|http_server_files:serve_files_in_directory(js)|=    |
| =|pldoc(.)|=            | =|pldoc_root|=                                        |
| =|pldoc(doc)|=          | |
| =|pldoc(doc_for)|=      | |
| =|pldoc(edit)|=         | |
| =|pldoc(file)|=         | |
| =|pldoc('index.html')|= | |
| =|pldoc(man)|=          | |
| =|pldoc(pack)|=         | |
| =|pldoc('pack/')|=      | |
| =|pldoc(place)|=        | |
| =|pldoc('res/')|=       | |
| =|pldoc(search)|=       | |
| =|pldoc_man(.)|=        | |
| =|pldoc_pkg(.)|=        | |


## Server rebase

In order to rebase an entire Web application:

```prolog
set_setting(http:prefix, Prefix).
```

See [http://www.swi-prolog.org/pldoc/doc_for?object=section%28%27packages/http.html%27%29].
