package org.lawrencebower.docgen.web_model.view.document.binding;

import org.springframework.util.AutoPopulatingList;

import java.util.List;

public class DataEntryBindBean {

    private List<DocComponentBindBean> components = new AutoPopulatingList<>(DocComponentBindBean.class);

    public List<DocComponentBindBean> getComponents() {
        return components;
    }

}
