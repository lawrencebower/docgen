package org.lawrencebower.docgen.web_model.view.document.binding;

import org.lawrencebower.docgen.web_model.view.document.component.TableComponentView;
import org.springframework.util.AutoPopulatingList;

import java.util.List;

public class TableComponentBindBean {

    private String tableName;
    private List<TableRowBindBean> rows = new AutoPopulatingList<>(TableRowBindBean.class);

    public String getTableName() {
        return tableName;
    }

    public void setTableName(String tableName) {
        this.tableName = tableName;
    }

    public List<TableRowBindBean> getRows() {
        return rows;
    }

    public void setRows(List<TableRowBindBean> rows) {
        this.rows = rows;
    }

    public void bindTableValues(TableComponentView tableComponentView) {
        for (TableRowBindBean bindRow : rows) {
            bindRow.bindRowValuesIfMatch(tableComponentView);
        }

    }

    public boolean isSet() {
        return tableName != null;
    }
}