package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedVendorPhone extends AutoMappedComponent {

    public static final String VENDOR_PHONE = "AUTO_MAPPED_VENDOR_PHONE";

    public AutoMappedVendorPhone() {
        name = VENDOR_PHONE;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getVendorPhone();
        setComponentValueIfMatch(docComponentView, value);
    }
}
