package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedVendorTaxId extends AutoMappedComponent {

    public static final String VENDOR_TAX_ID = "AUTO_MAPPED_VENDOR_TAX_ID";

    public AutoMappedVendorTaxId() {
        name = VENDOR_TAX_ID;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getVendorTaxId();
        setComponentValueIfMatch(docComponentView, value);
    }
}
