package org.lawrencebower.docgen.web_model.business_def.table_component;

import org.lawrencebower.docgen.web_model.view.document.component.TableComponentView;

public interface TableComponentValueSetter {
    void setCellValueIfMatch(String paramString,
                             String componentValue,
                             TableComponentView tableView);

    boolean checkComponentNameMatch(String paramString, String componentName);
}
