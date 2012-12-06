package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedBusinessAddress extends AutoMappedComponent {

    public AutoMappedBusinessAddress() {
        name = BUSINESS_ADDRESS;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getBusinessAddress();
        setComponentValueIfMatch(docComponentView, value);
    }
}
