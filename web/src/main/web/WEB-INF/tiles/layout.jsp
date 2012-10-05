<%@ include file="/WEB-INF/jsp/include.jsp" %>

<html>
<head>
    <title><tiles:getAsString name="title"/></title>
</head>
<body>
<div id="header" align="center">
    <div id="headerTitle"><tiles:insertAttribute name="header"/></div>
</div>
<div id="content" align="center">
    <tiles:insertAttribute name="body"/>
</div>
</body>
</html>