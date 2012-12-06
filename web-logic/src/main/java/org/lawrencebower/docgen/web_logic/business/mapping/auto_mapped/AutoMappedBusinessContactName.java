package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedBusinessContactName extends AutoMappedComponent {

    public static final String BUSINESS_CONTACT_NAME = "AUTO_MAPPED_BUSINESS_CONTACT_NAME";

    public AutoMappedBusinessContactName() {
        name = BUSINESS_CONTACT_NAME;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getBusinessContactName();
        setComponentValueIfMatch(docComponentView, value);
    }
}
