package org.lawrencebower.docgen.web_model.business_def.component_calculation.table;

import org.lawrencebower.docgen.web_model.view.document.DocumentSet;
import org.lawrencebower.docgen.web_model.view.document.component.TableComponentView;

import java.util.List;

public interface TableComponentCalculator {

    void runCalculations(TableComponentView tableComponentView,
                         List<TableComponentCalculation> componentCalculations,
                         DocumentSet documentSet);

}
