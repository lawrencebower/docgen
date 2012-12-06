package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedVendorAddress extends AutoMappedComponent {

    public AutoMappedVendorAddress() {
        name = VENDOR_ADDRESS;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getVendorAddress();
        setComponentValueIfMatch(docComponentView, value);
    }
}
