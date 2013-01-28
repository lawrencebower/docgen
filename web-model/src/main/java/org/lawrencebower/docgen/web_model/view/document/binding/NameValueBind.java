package org.lawrencebower.docgen.web_model.view.document.binding;

import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.table.view_table.ViewCell;
import org.lawrencebower.docgen.core.document.component.table.view_table.ViewRow;

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

    public void bindCellValueIfMatch(ViewRow tableRow) {

        List<ViewCell> tableCells = tableRow.getCells();

        for (ViewCell tableCell : tableCells) {
            String cellName = tableCell.getName();
            if(name.equals(cellName)){
                TextComponent component = (TextComponent) tableCell.getComponent();
                component.setText(value);
            }
        }
    }
}
