package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedVendorCountry extends AutoMappedComponent {

    public static final String VENDOR_COUNTRY = "AUTO_MAPPED_VENDOR_COUNTRY";

    public AutoMappedVendorCountry() {
        name = VENDOR_COUNTRY;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getVendorCountry();
        setComponentValueIfMatch(docComponentView, value);
    }
}
