package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedBusinessPhone extends AutoMappedComponent {

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
