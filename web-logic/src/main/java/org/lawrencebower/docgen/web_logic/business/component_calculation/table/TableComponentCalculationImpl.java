package org.lawrencebower.docgen.web_logic.business.component_calculation.table;

import org.lawrencebower.docgen.web_logic.business.component_calculation.ComponentCalculationImpl;
import org.lawrencebower.docgen.web_logic.business.component_calculation.Format;
import org.lawrencebower.docgen.web_logic.business.component_calculation.Operator;
import org.lawrencebower.docgen.web_logic.view.document.component.TableComponentCalculation;

public class TableComponentCalculationImpl extends ComponentCalculationImpl implements TableComponentCalculation {

    private String targetColumn;

    public TableComponentCalculationImpl(Operator operator,
                                         String targetColumn,
                                         String... fields) {
        super(operator, fields);
        this.targetColumn = targetColumn;
    }

    public TableComponentCalculationImpl(Operator operator,
                                         Format format,
                                         String targetColumn,
                                         String... fields) {
        super(operator, format, fields);
        this.targetColumn = targetColumn;
    }

    @Override
    public String getTargetColumn() {
        return targetColumn;
    }
}
