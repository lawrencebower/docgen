package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedBusinessName extends AutoMappedComponent {

    public static final String BUSINESS_NAME = "AUTO_MAPPED_BUSINESS_NAME";

    public AutoMappedBusinessName() {
        name = BUSINESS_NAME;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getBusinessName();
        setComponentValueIfMatch(docComponentView, value);
    }
}
