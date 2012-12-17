package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AutoMappedComponentInfo;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public interface AutoMapped {

    void mapComponent(DocComponentView docComponentView,
                      AutoMappedComponentInfo info);

    boolean matchesName(String name);
}
