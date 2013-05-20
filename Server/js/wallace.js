function clickme(name) {
  //document.write(name);
  var xhr = createXHR();
  if(xhr) {
    xhr.open("GET", "node?id=" + name, true);
  }
  xhr.send(null);
}

function continuous(name) {
  window.setInterval(function() { updateCommand(name); }, 1000);
}

function createXHR() {
  try {
    return new XMLHttpRequest();
  } catch(e) {
    // No catching yet.
  }
  window.alert("XMLHttpRequest not supported");
  return null;
}

function handleResponse(name, xhr) {
  if (xhr.readyState == 4 && xhr.status == 200) {
    var content = xhr.responseText;
    var container = window.document.getElementById(name);
    container.innerHTML = content;
  }
}

function updateCommand(name) {
  var xhr = createXHR();
  if(xhr) {
    xhr.open("GET", name, true);
    xhr.onreadystatechange = function () {
      handleResponse(name, xhr);
    };
    xhr.send(null)
  }
}

