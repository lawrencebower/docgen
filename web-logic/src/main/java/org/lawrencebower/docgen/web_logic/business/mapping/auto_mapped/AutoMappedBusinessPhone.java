package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedBusinessPhone extends AutoMappedComponent {

    public static final String BUSINESS_PHONE = "AUTO_MAPPED_BUSINESS_PHONE";

    public AutoMappedBusinessPhone() {
        name = BUSINESS_PHONE;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getBusinessPhone();
        setComponentValueIfMatch(docComponentView, value);
    }
}
