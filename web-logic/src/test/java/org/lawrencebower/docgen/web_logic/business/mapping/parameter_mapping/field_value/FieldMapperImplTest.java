package org.lawrencebower.docgen.web_logic.business.mapping.parameter_mapping.field_value;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponentFactory;
import org.lawrencebower.docgen.web_model.view.document.DocumentSet;
import org.lawrencebower.docgen.web_model.view.document.DocumentSetFactory;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewFactory;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentViewFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.*;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-logic-test-context.xml")
public class FieldMapperImplTest {

    @Autowired
    org.lawrencebower.docgen.web_model.business_def.mapping.parameter_mapping.field_value.FieldMapper fieldMapper;
    @Autowired
    CustomComponentFactory customComponentFactory;
    @Autowired
    DocComponentViewFactory componentViewFactory;
    @Autowired
    DocumentViewFactory documentViewFactory;
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
        component1 = mock(TextComponent.class);
    }

    @Test
    public void testMapFieldValuesToComponents_validParams_correctComponentSet() {

        Map<String, String[]> paramMap = makeParameterMap();

        //define mock behaviour
        when(component1.getName()).thenReturn(fieldName1);
        component2 = mock(TextComponent.class);
        when(component2.getName()).thenReturn("some value not in the params");
        component3 = mock(TextComponent.class);
        when(component3.getName()).thenReturn("some value not in the params");
        component4 = mock(TextComponent.class);
        when(component4.getName()).thenReturn(fieldName1);

        //add mocks to Documents
        DocumentSet documentSet = addMocksToDocuments();
        List<DocComponentView> allComponents = documentSet.getAllComponentViewsFromDocs();

        //run mapping
        fieldMapper.mapFieldValuesToComponents(paramMap, allComponents);

        //verify
        verify(component1, times(1)).setText(value1);

        verify(component2, atLeastOnce()).getName();
        verify(component2, never()).setText(anyString());

        verify(component3, atLeastOnce()).getName();
        verify(component3, never()).setText(anyString());

        verify(component4, times(1)).setText(value1);
    }

    @Test
    public void testMapFieldValuesToComponents_excludedField_excludedFieldSkipped() {

        String excludedTokenName = "full";
        Map<String, String[]> paramMap = makeParameterMap();
        paramMap.put(excludedTokenName,new String[]{"this value should not be set"});


        //define mock behaviour
        when(component1.getName()).thenReturn(fieldName1);
        component2 = mock(TextComponent.class);
        when(component2.getName()).thenReturn(excludedTokenName);
        component3 = mock(TextComponent.class);
        when(component3.getName()).thenReturn("some value not in the params");
        component4 = mock(TextComponent.class);
        when(component4.getName()).thenReturn("some value not in the params");

        //add mocks to Documents
        DocumentSet documentSet = addMocksToDocuments();
        List<DocComponentView> allComponents = documentSet.getAllComponentViewsFromDocs();

        //run mapping
        fieldMapper.mapFieldValuesToComponents(paramMap, allComponents);

        //verify
        verify(component1, times(1)).setText(value1);
        verify(component2, atLeastOnce()).getName();
        verify(component2, never()).setText(anyString());
        verify(component3, atLeastOnce()).getName();
        verify(component3, never()).setText(anyString());
        verify(component4, atLeastOnce()).getName();
        verify(component4, never()).setText(anyString());
    }

    @Test
    public void testMapFieldValuesToComponents_missingValuesInParamMap_throwsError() {

        try {
            Map<String, String[]> paramMap = makeParameterMap();
            paramMap.put("token with no values", new String[]{});

            //define mock behaviour
            when(component1.getName()).thenReturn(fieldName1);
            component2 = mock(TextComponent.class);
            when(component2.getName()).thenReturn(fieldName2);
            component3 = mock(TextComponent.class);
            when(component3.getName()).thenReturn(fieldName3);
            component4 = mock(TextComponent.class);
            when(component4.getName()).thenReturn(fieldName1);

            //add mocks to Documents
            DocumentSet documentSet = addMocksToDocuments();
            List<DocComponentView> allComponents = documentSet.getAllComponentViewsFromDocs();

            //run mapping
            fieldMapper.mapFieldValuesToComponents(paramMap, allComponents);
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals("No values bound to field 'token with no values'", message);
        }
    }

    @Test
    public void testMapFieldValuesToComponents_moreThanOneBoundValueInParamMap_throwsError() {

        try {
            Map<String, String[]> paramMap = makeParameterMap();
            paramMap.put("token with no values", new String[]{"value1","value2"});

            //define mock behaviour
            when(component1.getName()).thenReturn(fieldName1);
            component2 = mock(TextComponent.class);
            when(component2.getName()).thenReturn(fieldName2);
            component3 = mock(TextComponent.class);
            when(component3.getName()).thenReturn(fieldName3);
            component4 = mock(TextComponent.class);
            when(component4.getName()).thenReturn(fieldName1);

            //add mocks to Documents
            DocumentSet documentSet = addMocksToDocuments();
            List<DocComponentView> allComponents = documentSet.getAllComponentViewsFromDocs();

            //run mapping
            fieldMapper.mapFieldValuesToComponents(paramMap, allComponents);
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals("more than 1 value bound to field 'token with no values'", message);
        }
    }

    //UTIL METHODS//

    private DocumentSet addMocksToDocuments() {
        DocumentView documentView1 = makeDocumentView();
        addComponentViewToDoc(documentView1, component1);
        addComponentViewToDoc(documentView1, component2);

        DocumentView documentView2 = makeDocumentView();
        addComponentViewToDoc(documentView2, component3);
        addComponentViewToDoc(documentView2, component4);

        return documentSetFactory.createDocumentInfoSet(documentView1, documentView2);
    }

    private void addComponentViewToDoc(DocumentView documentView, TextComponent component) {
        DocComponentView componentView = componentViewFactory.createTextComponentView(component);
        documentView.addComponentView(componentView);
    }

    private DocumentView makeDocumentView() {
        return documentViewFactory.createDocumentView();
    }

    private Map<String, String[]> makeParameterMap() {
        Map<String, String[]> paramMap = new HashMap<>();
        paramMap.put(fieldName1, new String[]{value1});
        paramMap.put(fieldName2, new String[]{value2});
        paramMap.put(fieldName3, new String[]{value3});
        return paramMap;
    }

}
