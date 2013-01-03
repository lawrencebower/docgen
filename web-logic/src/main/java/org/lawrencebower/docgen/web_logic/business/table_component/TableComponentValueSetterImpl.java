package org.lawrencebower.docgen.web_logic.business.table_component;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.business_def.table_component.TableComponentValueSetter;
import org.lawrencebower.docgen.web_logic.view.constants.ViewConstants;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_logic.view.document.component.TableComponentView;

public class TableComponentValueSetterImpl implements TableComponentValueSetter {

    private TableComponentInfo parseParamString(String paramString, String componentValue) {
        String[] tokens = splitTableParamString(paramString);

        int rowNumber = Integer.parseInt(tokens[1]);
        rowNumber--;//decrement as componentValue from jsp starts at 1, rather than 0
        int colNumber = Integer.parseInt(tokens[2]);
        colNumber--;//decrement as componentValue from jsp starts at 1, rather than 0

        return new TableComponentInfo(componentValue, rowNumber, colNumber);
    }

    @Override
    public void setCellValueIfMatch(String paramString,
                                    String componentValue,
                                    TableComponentView tableView) {

        String componentName = tableView.getName();
        boolean componentNameMatches = checkComponentNameMatch(paramString, componentName);

        if (componentNameMatches) {//is this the right component

            TableComponentInfo tableComponentInfo = parseParamString(paramString, componentValue);

            int rowNum = tableComponentInfo.getRowNum();
            int colNum = tableComponentInfo.getColNum();
            String value = tableComponentInfo.getValue();

            DocComponentView componentView = tableView.getCellComponentView(rowNum, colNum);

            componentView.setComponentValue(value);
        }
    }

    private String[] splitTableParamString(String paramString) {

        String[] tokens = paramString.split(ViewConstants.DOCUMENT_FIELD_SEPARATOR);

        if (tokens.length != 3) {
            String messageTemplate = "Unexpected number of tokens when parsing table cell param String '%s'";
            String message = String.format(messageTemplate, paramString);
            throw new DocGenException(message);
        }

        return tokens;
    }

    @Override
    public boolean checkComponentNameMatch(String paramString, String componentName) {
        return paramString.startsWith(componentName + ViewConstants.DOCUMENT_FIELD_SEPARATOR);
    }

    private class TableComponentInfo {
        private String value;
        private int rowNum;
        private int colNum;

        public TableComponentInfo(String value, int rowNum, int colNum) {
            this.value = value;
            this.rowNum = rowNum;
            this.colNum = colNum;
        }

        public String getValue() {
            return value;
        }

        public int getRowNum() {
            return rowNum;
        }

        public int getColNum() {
            return colNum;
        }
    }
}
