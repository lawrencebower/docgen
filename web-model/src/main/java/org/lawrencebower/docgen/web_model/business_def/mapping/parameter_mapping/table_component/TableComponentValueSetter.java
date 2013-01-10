package org.lawrencebower.docgen.web_model.business_def.mapping.parameter_mapping.table_component;

import org.lawrencebower.docgen.web_model.view.document.component.TableComponentView;

public interface TableComponentValueSetter {
    void setCellValueIfMatch(String paramString,
                             String componentValue,
                             TableComponentView tableView);

    boolean checkComponentNameMatch(String paramString, String componentName);
}
