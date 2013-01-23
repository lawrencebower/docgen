package org.lawrencebower.docgen.web_model.view.document.binding;

import org.lawrencebower.docgen.core.document.component.table.TableRow;
import org.lawrencebower.docgen.web_model.view.document.component.TableComponentView;
import org.springframework.util.AutoPopulatingList;

import java.util.List;

public class TableRowBindBean {

    private String rowName;
    private List<NameValueBind> cells = new AutoPopulatingList<>(NameValueBind.class);

    public String getRowName() {
        return rowName;
    }

    public void setRowName(String rowName) {
        this.rowName = rowName;
    }

    public List<NameValueBind> getCells() {
        return cells;
    }

    public void setCells(List<NameValueBind> cells) {
        this.cells = cells;
    }

    public void bindRowValuesIfMatch(TableComponentView tableView) {
        for (TableRow tableRow : tableView.getTableRows()) {
            String tableRowName = tableRow.getRowName();
            if (tableRowName.equals(rowName)) {
                bindCellsIfMatch(tableRow);
            }
        }
    }

    private void bindCellsIfMatch(TableRow tableRow) {
        for (NameValueBind bindCell : cells) {
            bindCell.bindCellValueIfMatch(tableRow);
        }
    }
}