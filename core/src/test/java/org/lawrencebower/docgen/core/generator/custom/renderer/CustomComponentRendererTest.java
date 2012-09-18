package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.*;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.mockito.Mockito;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.io.ByteArrayOutputStream;

import static org.junit.Assert.fail;
import static org.mockito.Mockito.verify;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/core-test-context.xml"})
public class CustomComponentRendererTest implements ApplicationContextAware {

    private CustomComponentRenderer renderer;

    private CustomTextRenderer mockTextRenderer = Mockito.mock(CustomTextRenderer.class);
    private CustomTableRenderer mockTableRenderer = Mockito.mock(CustomTableRenderer.class);
    private CustomNewLineRenderer mockNewLineRenderer = Mockito.mock(CustomNewLineRenderer.class);
    private CustomImageRenderer mockImageRenderer = Mockito.mock(CustomImageRenderer.class);
    private CustomTableTextRenderer mockTableTextRenderer = Mockito.mock(CustomTableTextRenderer.class);
    private CustomLineRenderer mockLineRenderer = Mockito.mock(CustomLineRenderer.class);
    private ApplicationContext applicationContext;

    @Before
    public void setup(){
        renderer = (CustomComponentRenderer) applicationContext.getBean("customComponentRenderer");
    }

    @Test
    public void testCreateAndRenderComponent_passUnknownComponent_throwsError() throws Exception {

        DocComponent unknownComponent = getUnknownComponent();

        CustomComponentRendererInfo rendererInfo = new CustomComponentRendererInfo(new ByteArrayOutputStream());

        try {
            renderer.createAndRenderComponent(unknownComponent, rendererInfo);
        } catch (DocGenException e) {
            return;
        }

        fail();//should not get here
    }

    @Test
    public void testCreateAndRenderComponent_passTableTextComponent_throwsError() throws Exception {

        TableTextComponent tableText = new TableTextComponent("text");

        CustomComponentRendererInfo rendererInfo = new CustomComponentRendererInfo(new ByteArrayOutputStream());

        try {
            renderer.createAndRenderComponent(tableText, rendererInfo);
        } catch (NotImplementedException e) {
            return;
        }

        fail();//should not get here
    }

    @Test
    public void testCreateAndRenderComponent_passCheckboxComponent_throwsError() throws Exception {

        DocComponent checkboxComponent = new CheckBoxComponent();

        CustomComponentRendererInfo rendererInfo = new CustomComponentRendererInfo(new ByteArrayOutputStream());

        try {
            renderer.createAndRenderComponent(checkboxComponent, rendererInfo);
        } catch (UnsupportedOperationException e) {
            return;
        }

        fail();//should not get here
    }

    @Test
    public void testCreateComponent_passUnknownComponent_throwsError() throws Exception {

        DocComponent unknownComponent = getUnknownComponent();

        try {
            renderer.createComponent(unknownComponent);
        } catch (DocGenException e) {
            return;
        }

        fail();//should not get here
    }

    @Test
    public void testCreateComponent_passCheckboxComponent_throwsError() throws Exception {

        DocComponent checkboxComponent = new CheckBoxComponent();

        try {
            renderer.createComponent(checkboxComponent);
        } catch (UnsupportedOperationException e) {
            return;
        }

        fail();//should not get here
    }

    private DocComponent getUnknownComponent() {
        return new DocComponent() {
            @Override
            public DocComponentType getComponentType() {
                return null;
            }
        };
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }
}
