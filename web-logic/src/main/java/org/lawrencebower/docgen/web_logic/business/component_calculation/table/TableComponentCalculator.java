package org.lawrencebower.docgen.web_logic.business.component_calculation.table;

import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoSet;
import org.lawrencebower.docgen.web_logic.view.document_info.component.TableComponentView;

import java.util.List;

public interface TableComponentCalculator {

    void runCalculations(TableComponentView tableComponentView,
                         List<TableComponentCalculation> componentCalculations,
                         DocumentInfoSet documentSet);

}
