package org.lawrencebower.docgen.web_logic.view.document.component;

public interface TableComponentValueSetter {
    void setCellValueIfMatch(String paramString,
                             String componentValue,
                             TableComponentView tableView);

    boolean checkComponentNameMatch(String paramString, String componentName);
}
