$(document).ready(function() {
  // Array of sidebar menu item IDs
  var new_els = ['welcome', 'client_counts', 'coc_competition', 'ceaps', 'utilization', 'qpr', 'spm', 'performance_summary', 'program_details', 'performance_summary_youth', 'youth_program_details', 'about'];

  // Iterate through the array of IDs
  new_els.forEach(function(id) {
    var checkExist = setInterval(function() {
      var el = $('#tab-' + id); // Select the sidebar tab element

      // If the element exists
      if (el.length) {
        // Add 'text-success' to the icon within the element
        el.children("i.fas").addClass('text-success');

        // If it's a parent item (like Ohio BoS Performance or Ohio Youth Performance), also color the parent icon
        if (id === 'qpr' || id === 'spm' || id === 'performance_summary' || id === 'program_details' || id === 'performance_summary_youth' || id === 'youth_program_details') {
          el.parents(".nav-item.has-treeview").find("i.fas").first().addClass('text-success');
        }

        // Clear the interval to stop checking
        clearInterval(checkExist);
      }
    }, 100); // Check every 100ms until the element is found
  });
});
