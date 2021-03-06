package org.lawrencebower.docgen.web_logic.business.mapping.parameter_mapping.field_value;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.DocComponentType;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponentFactory;
import org.lawrencebower.docgen.web_model.business_def.mapping.parameter_mapping.field_value.FieldMapper;
import org.lawrencebower.docgen.web_model.view.document.DocumentSet;
import org.lawrencebower.docgen.web_model.view.document.DocumentSetFactory;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewBuilder;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewImpl;
import org.lawrencebower.docgen.web_model.view.document.binding.DataEntryBindBean;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentViewFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.List;

import static org.mockito.Mockito.*;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-logic-test-context.xml")
public class FieldMapperImplTest {

    @Autowired
    FieldMapper fieldMapper;
    @Autowired
    CustomComponentFactory customComponentFactory;
    @Autowired
    DocComponentViewFactory componentViewFactory;
    @Autowired
    DocumentViewBuilder documentViewBuilder;
    @Autowired
    DocumentSetFactory documentSetFactory;

    private final String fieldName1 = "fieldName1";
    private final String fieldName2 = "fieldName2";
    private final String fieldName3 = "fieldName3";

    private final String value1 = "value1";
    private final String value2 = "value2";
    private final String value3 = "value3";

    TextComponent component1;
    TextComponent component2;
    TextComponent component3;
    TextComponent component4;

    @Before
    public void setup() {
        component1 = mockTextField();
        component2 = mockTextField();
        component3 = mockTextField();
        component4 = mockTextField();
    }

    private TextComponent mockTextField() {
        TextComponent component = mock(TextComponent.class);
        when(component.getComponentType()).thenReturn(DocComponentType.TEXT);
        return component;
    }

    @Test
    public void testMapFieldValuesToComponents_validParams_correctComponentSet() {

        DataEntryBindBean bindBean = makeBindingBean();

        //define mock behaviour
        when(component1.getName()).thenReturn(fieldName1);
        when(component2.getName()).thenReturn("some value not in the params");
        when(component3.getName()).thenReturn("some value not in the params");
        when(component4.getName()).thenReturn(fieldName1);

        //add mocks to Documents
        DocumentSet documentSet = addMocksToDocuments();
        List<DocComponentView> allComponents = documentSet.getAllComponentViewsFromDocs();

        //run mapping
        fieldMapper.mapFieldValuesToComponents(bindBean, allComponents);

        //verify
        verify(component1, times(1)).setText(value1);

        verify(component2, atLeastOnce()).getName();
        verify(component2, never()).setText(anyString());

        verify(component3, atLeastOnce()).getName();
        verify(component3, never()).setText(anyString());

        verify(component4, times(1)).setText(value1);
    }

    //UTIL METHODS//

    private DocumentSet addMocksToDocuments() {
        documentViewBuilder.createDocument();
        documentViewBuilder.addViewableComponent(component1);
        documentViewBuilder.addViewableComponent(component2);
        DocumentViewImpl documentView1 = documentViewBuilder.getDocumentView();

        documentViewBuilder.createDocument();
        documentViewBuilder.addViewableComponent(component3);
        documentViewBuilder.addViewableComponent(component4);
        DocumentViewImpl documentView2 = documentViewBuilder.getDocumentView();

        return documentSetFactory.createDocumentInfoSet(documentView1, documentView2);
    }

    private DataEntryBindBean makeBindingBean() {

        //autopopulating bean
        DataEntryBindBean bindBean = new DataEntryBindBean();

        bindBean.getComponents().get(0).setName(fieldName1);
        bindBean.getComponents().get(0).setValue(value1);

        bindBean.getComponents().get(1).setName(fieldName2);
        bindBean.getComponents().get(1).setValue(value2);

        bindBean.getComponents().get(2).setName(fieldName3);
        bindBean.getComponents().get(2).setValue(value3);

        return bindBean;
    }

}
