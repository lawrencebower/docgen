package org.lawrencebower.docgen.web_logic.business.component_calculation.table;

import org.lawrencebower.docgen.web_logic.business.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_logic.business.component_calculation.Format;
import org.lawrencebower.docgen.web_logic.business.component_calculation.Operator;

public class TableComponentCalculation extends ComponentCalculation {

    private String targetColumn;

    public TableComponentCalculation(Operator operator,
                                     String targetColumn,
                                     String... fields) {
        super(operator, fields);
        this.targetColumn = targetColumn;
    }

    public TableComponentCalculation(Operator operator,
                                     Format format,
                                     String targetColumn,
                                     String... fields) {
        super(operator, format, fields);
        this.targetColumn = targetColumn;
    }

    public String getTargetColumn() {
        return targetColumn;
    }
}
