$(document).on('click', '.workflowclass', function () {
Shiny.onInputChange('workflowselected',this.id);
// to report changes on the same selectInput
Shiny.onInputChange('workflowclassrand', Math.random());
});