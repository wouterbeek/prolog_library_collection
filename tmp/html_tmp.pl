:- module(
  html_tmp,
  [
    html_pagination//2, % ?NumPages, +Page
  ]
).

/** <module> HTML temporary

HTML generators that are currently unused.

@author Wouter Beek
@version 2016/09
*/

:- use_module(library(http/html_write)).





%! dropbox_plugin(+Key)// is det.

dropbox_plugin(Key) -->
  html([
    div(id=dropboxBtn, []),
    div(class=submitStatusDropbox, []),
    script([
      'data-app-key'=Key,
      id=dropboxjs,
      src='https://www.dropbox.com/static/api/2/dropins.js',
      type='text/javascript'
    ], []),
    \js_script({|javascript(_)||
$(document).ready(function() {
  var button = Dropbox.createChooseButton({success: function(files) {
    if (files.length > 0) {
      var url = files[0].link.replace('dl=0', 'dl=1');
      $.ajax({
        data: {url: url},
        error: function(response, textStatus, errorThrown) {
          var errorThrown = response.responseText || errorThrown;
          try {
            errorThrown = JSON.parse(errorThrown);
          } catch(e) {
            // Never mind, it is just a string.
          }
          var msg = "Something went wrong.";
          if (errorThrown && typeof errorThrown == "string" && errorThrown.length > 0) {
            msg = errorThrown + '.';
          }
          $(".submitStatusDropbox").empty().hide().append("<span class=\"label label-danger\">" + msg + " If this problem persists, please drop us a <a style=\"color:#2C2C2C\" href=\"https://github.com/LODLaundry/lodlaundry.github.io/issues\">Github issue</a>!</span>").show(400);
        },
        success: function() {
          $(".submitStatusDropbox").empty().hide().append(successMsg).show(400);
          //update table as well
          $('#urlFilter').val(url);
          doSearch();
        },
        type: "GET",
        url: api.laundryBasket.seedUpdateApi
      });
    }
  }});
  document.getElementById("dropboxBtn").appendChild(button);
});

var watchDoc = function(el) {
  // Remove previous notifications, if any.
  $('.notifyAlert').remove();
  var $el = $(el);
  var $metaBtn = ($el.closest('tr').find('.meta-info'));
  if ($metaBtn.length > 0) {
    var doc = $metaBtn.attr('href');
    var email = getCookie('email');
    // Remove previous used ones.
    $el.editable({value: email, onBlur: submit});
    $el.on('shown', function(e, editable) {
      editable.input.$input.attr('placeholder', 'Enter email address');
    });
    $el.on('hidden', function(event, type){
      if (type != 'cancel' && type != 'onblur') {
        var email = $el.editable('getValue', true).trim();
        if (email.length > 0) {
          setCookie('email', email);
          $.ajax({
            url: api.notifications.api + '/watch',
            data: {doc: doc, email: email},
            error: function(response,textStatus,errorThrown) {
              $el.closest('td').append($('<span>', {'class': 'label label-danger notifyAlert'}).text(response.responseText || errorThrown));
              console.log(response);
            },
            success: function(data, textStatus, jqXhr) {
              $el.closest('td').append($('<span>', {'class': 'label label-success notifyAlert'}).text("Now watching for changes"));
              $el.remove();
            },
          });
        }
      }
      $(el).editable('destroy');
    });
    $(el).editable('show');
  }
}
    |})
  ]).



%! pagination(?NumPages, +Page)// is det.

pagination(NumPages, Page) -->
  {var(NumPages)}, !,
  html(
    nav(
      ul(class=pagination, [
        \html_previous_page,
        li(a(href='#', Page)),
        \html_next_page
      ])
    )
  ).
pagination(NumPages, _Page) -->
  html(
    nav(
      ul(class=pagination, [
        \previous_page,
        \pages(NumPages, 3),
        \next_page
      ])
    )
  ).


previous_page -->
  html(
    li(
      a(['aria-label'='Previous',href='#'],
        span('aria-hidden'=true, &(laquo))
      )
    )
  ).


pages(NumPages, Width) -->
  {NumPages > Width * 2 + 1}, !,
  {NumPages0 is NumPages - Width},
  html([
    \pages0(0, Width),
    li(a(href='', "…")),
    \pages0(NumPages0, NumPages)
  ]).
pages(NumPages, _) -->
  pages0(0, NumPages).


pages0(NumPages, NumPages) --> !, [].
pages0(N1, NumPages) -->
  {N2 is N1 + 1},
  html(li(a(href='#', N1))),
  pages0(N2, NumPages).


next_page -->
  html(
    li(
      a(['aria-label'='Next',href='#'],
        span('aria-hidden'=true, &(raquo))
      )
    )
  ).
