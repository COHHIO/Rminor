$( '.main-sidebar' ).ready(function() {
  var new_els = ['welcome','client_counts','coc_competition','ceaps','utilization','performance_summary','qpr_community_need','qpr_length_of_stay','qpr_permanent_housing','qpr_temp_permanent_housing','qpr_noncash_benefits','qpr_health_insurance','qpr_income_growth','qpr_rrh_placement','qpr_rrh_spending','qpr_reentries','performance_summary_youth','qpr_community_need_youth', 'qpr_length_of_stay_youth', 'qpr_permanent_housing_youth','qpr_temp_permanent_housing_youth','qpr_noncash_benefits_youth','qpr_health_insurance_youth','qpr_income_growth_youth','qpr_rrh_placement_youth','qpr_rrh_spending_youth','spm','about']
  new_els.map(function(id) {
    var checkExist = setInterval(function() {
   if ($('#'+"tab-"+ id).length) {
      var el = $("#tab-" + id)
      el.children("i.fas").addClass('text-success')
      if (id.match('^qpr|^dq'))
        var a = el.parents(".nav-item.has-treeview").find("i.fas").first().addClass('text-success')
      clearInterval(checkExist);
   }
}, 100); 
  })
})