Shiny.addCustomMessageHandler("imageReady",
  function(message) {
    console.log(message);
    document.getElementById(message.id).click();
  }
);
