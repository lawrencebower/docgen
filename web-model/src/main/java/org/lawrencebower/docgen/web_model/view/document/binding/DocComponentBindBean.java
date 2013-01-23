package org.lawrencebower.docgen.web_model.view.document.binding;

import org.lawrencebower.docgen.web_model.view.document.component.TableComponentView;

public class DocComponentBindBean {

    private NameValueBind nameValueBindBean = new NameValueBind();
    private TableComponentBindBean tableData = new TableComponentBindBean();

    public String getName() {
        return nameValueBindBean.getName();
    }

    public void setName(String name) {
        nameValueBindBean.setName(name);
    }

    public String getValue() {
        return nameValueBindBean.getValue();
    }

    public void setValue(String value) {
        nameValueBindBean.setValue(value);
    }

    public TableComponentBindBean getTableData() {
        return tableData;
    }

    public void setTableCellValuesIfMatch(TableComponentView tableComponentView) {

        String tableComponentName = tableComponentView.getName();

        boolean match = checkComponentNameMatch(tableComponentName);

        if(match){
            tableData.bindTableValues(tableComponentView);
        }
    }

    public boolean checkComponentNameMatch(String componentName) {

        boolean match = false;

        TableComponentBindBean tableData = getTableData();
        if(tableData.isSet()){
            String bindName = tableData.getTableName();
            match = bindName.equals(componentName);
        }

        return match;
    }

}
