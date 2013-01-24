package org.lawrencebower.docgen.web_model.view.document.binding;

import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.table.view_table.ViewTableCell;
import org.lawrencebower.docgen.core.document.component.table.view_table.ViewTableRow;

import java.util.List;

public class NameValueBind {

    private String name;
    private String value;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public void bindCellValueIfMatch(ViewTableRow tableRow) {

        List<ViewTableCell> tableCells = tableRow.getCells();

        for (ViewTableCell tableCell : tableCells) {
            String cellName = tableCell.getName();
            if(name.equals(cellName)){
                TextComponent component = (TextComponent) tableCell.getComponent();
                component.setText(value);
            }
        }
    }
}
