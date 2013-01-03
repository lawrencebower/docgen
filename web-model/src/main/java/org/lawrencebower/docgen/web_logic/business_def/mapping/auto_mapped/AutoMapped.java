package org.lawrencebower.docgen.web_logic.business_def.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.business_def.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public interface AutoMapped {

    void mapComponent(DocComponentView docComponentView,
                      AMComponentInfo info);

    boolean matchesName(String name);
}
