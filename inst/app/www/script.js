$( document ).ready(function() {
Shiny.addCustomMessageHandler('getDropdownValues', function(dropdown_ids) {
  var result = {};
  dropdown_ids.forEach(function(id) {
    var el = document.getElementById(id);
    result[id] = el ? el.value : null;
  });
  console.log("Sending dropdown_values:", result);
  Shiny.setInputValue('modal_upload_1-dropdown_values', result, {priority: 'event'});
});
});
