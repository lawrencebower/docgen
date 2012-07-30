package org.lawrencebower.docgen.core.generator.custom;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/integration-test-config.xml"})
public class CustomComponentRendererInfoIntegrationTest {

    private CustomComponentRendererInfo rendererInfo;

    @Before
    public void setUp(){
        this.rendererInfo = new CustomComponentRendererInfo();
    }

    @Test
    public void testPreparePDFWriter_nullOutputStream_throwsError() throws Exception {
        rendererInfo.preparePDFWriter(null);
    }

    @Test
    public void testMakeNewDocument() throws Exception {

    }

    @Test
    public void testCloseDocument() throws Exception {

    }

    @Test
    public void testAddToDocument() throws Exception {

    }
}
