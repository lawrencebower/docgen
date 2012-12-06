package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedVendorNameAndAddress extends AutoMappedComponent {

    private final String VENDOR_NAME_AND_ADDRESS = "AUTO_MAPPED_VENDOR_NAME_AND_ADDRESS";

    public AutoMappedVendorNameAndAddress() {
        name = VENDOR_NAME_AND_ADDRESS;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String vendorName = info.getVendorName();
        String vendorAddress = info.getVendorAddress();
        String value = vendorName + "\n" + vendorAddress;
        setComponentValueIfMatch(docComponentView, value);
    }
}
