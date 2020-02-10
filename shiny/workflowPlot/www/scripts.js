$(document).on("click", ".workflowclass", function() {
  Shiny.onInputChange("workflowselected", this.id);
  // to report changes on the same selectInput
  Shiny.onInputChange("workflowclassrand", Math.random());
});

$(document).on("click", ".expanclass", function() {
  Shiny.onInputChange("workflows_explor_selected", this.id);
  // to report changes on the same selectInput
  Shiny.onInputChange("workflow_explor_classrand", Math.random());
});
