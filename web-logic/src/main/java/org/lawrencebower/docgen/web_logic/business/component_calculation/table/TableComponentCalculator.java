package org.lawrencebower.docgen.web_logic.business.component_calculation.table;

import org.lawrencebower.docgen.web_logic.view.document.DocumentSet;
import org.lawrencebower.docgen.web_logic.view.document.component.TableComponentView;

import java.util.List;

public interface TableComponentCalculator {

    void runCalculations(TableComponentView tableComponentView,
                         List<TableComponentCalculation> componentCalculations,
                         DocumentSet documentSet);

}
