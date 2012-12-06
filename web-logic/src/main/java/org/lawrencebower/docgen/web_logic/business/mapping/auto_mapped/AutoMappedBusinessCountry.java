package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedBusinessCountry extends AutoMappedComponent {

    public AutoMappedBusinessCountry() {
        name = BUSINESS_COUNTRY;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getBusinessCountry();
        setComponentValueIfMatch(docComponentView, value);
    }
}
