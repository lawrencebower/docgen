package org.lawrencebower.docgen.web_logic.view.document.component;

import org.lawrencebower.docgen.web_logic.view.document.DocumentSet;

import java.util.List;

public interface TableComponentCalculator {

    void runCalculations(TableComponentView tableComponentView,
                         List<TableComponentCalculation> componentCalculations,
                         DocumentSet documentSet);

}
