<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output
        doctype-system="about:legacy-compat"
        encoding="UTF-8"
        media-type="text/html"
        method="html"/>

    <xsl:variable name="baseUrl" select="normalize-space(/root/meta/baseUrl)"/>
    <xsl:variable name="clientId" select="normalize-space(/root/meta/clientId)"/>

    <xsl:template match="/root">
        <html lang="en-US">
            <head>
                <meta content="initial-scale = 1, width = device-width" name="viewport"/>
                <title>
                    Monadoc
                </title>
                <link href="{$baseUrl}{normalize-space(meta/routes/bootstrap)}" rel="stylesheet"/>
                <link
                    href="{$baseUrl}{normalize-space(meta/routes/favicon)}"
                    rel="icon"
                    type="image/svg+xml"/>
                <link rel="canonical" href="{$baseUrl}{normalize-space(meta/routes/self)}"/>
            </head>
            <body>
                <header class="mb-3">
                    <nav class="navbar navbar-dark bg-dark flex-nowrap">
                        <div class="container">
                            <a class="navbar-brand" href="{$baseUrl}">Monadoc</a>
                            <form action="{$baseUrl}{normalize-space(meta/routes/search)}" class="d-flex">
                                <input class="form-control me-1" name="query" type="search" placeholder="Search"/>
                                <button class="btn btn-outline-light" type="submit">Go</button>
                            </form>
                        </div>
                    </nav>
                    <nav class="navbar navbar-light bg-light">
                        <div class="container">
                            <ol class="breadcrumb mb-0">
                                <xsl:for-each select="meta/breadcrumbs/breadcrumb">
                                    <xsl:variable name="route" select="normalize-space(route)"/>

                                    <xsl:choose>
                                        <xsl:when test="$route">
                                            <li class="breadcrumb-item">
                                                <a href="{$baseUrl}{$route}">
                                                    <xsl:value-of select="name"/>
                                                </a>
                                            </li>
                                        </xsl:when>
                                        <xsl:otherwise>
                                            <li class="breadcrumb-item active">
                                                <xsl:value-of select="name"/>
                                            </li>
                                        </xsl:otherwise>
                                    </xsl:choose>
                                </xsl:for-each>
                            </ol>
                            <ul class="navbar-nav">
                                <li class="nav-item">
                                    <xsl:variable name="user" select="normalize-space(meta/user)"/>
                                    <xsl:choose>
                                        <xsl:when test="$user">
                                            <a class="nav-link" href="{$baseUrl}{normalize-space(meta/routes/account)}">@<xsl:value-of select="$user"/></a>
                                        </xsl:when>
                                        <xsl:otherwise>
                                            <a class="nav-link" href="https://github.com/login/oauth/authorize?client_id={$clientId}&amp;redirect_uri={$baseUrl}{normalize-space(meta/routes/callback)}&amp;state={normalize-space(meta/routes/self)}">Log in</a>
                                        </xsl:otherwise>
                                    </xsl:choose>
                                </li>
                            </ul>
                        </div>
                    </nav>
                </header>
                <main class="container mt-3 mb-3">
                    <xsl:apply-templates select="page"/>
                </main>
                <footer class="container pt-3 mt-3 text-muted border-top mb-5">
                    <p>
                        Powered by
                        <a href="https://github.com/tfausak/monadoc">Monadoc</a>
                        version
                        <xsl:value-of select="normalize-space(meta/version)"/>.
                    </p>
                </footer>
            </body>
        </html>
    </xsl:template>

    <xsl:template match="index">
        <p>
            &#x1f516; Worse Haskell documentation.
        </p>
        <h2>
            Recent Uploads
        </h2>
        <ul>
            <xsl:for-each select="packages/package">
                <xsl:variable name="revision" select="normalize-space(revision)"/>

                <li>
                    <code>
                        <xsl:value-of select="normalize-space(uploadedAt)"/>
                    </code>
                    <xsl:text>: </xsl:text>
                    <a href="{$baseUrl}{normalize-space(route)}">
                        <xsl:value-of select="normalize-space(name)"/>
                        <xsl:text>-</xsl:text>
                        <xsl:value-of select="normalize-space(version)"/>
                        <xsl:if test="$revision != 0">
                            <xsl:text>-</xsl:text>
                            <xsl:value-of select="$revision"/>
                        </xsl:if>
                    </a>
                </li>
            </xsl:for-each>
        </ul>
    </xsl:template>

    <xsl:template match="release">
        <xsl:variable name="name" select="normalize-space(package/name)"/>
        <xsl:variable name="version" select="normalize-space(package/version)"/>
        <xsl:variable name="revision" select="normalize-space(package/revision)"/>
        <xsl:variable name="preferred" select="boolean(normalize-space(package/preferred))"/>
        <xsl:variable name="isLatest" select="boolean(normalize-space(package/latest))"/>

        <xsl:if test="not($preferred)">
            <div class="alert alert-danger">
                Version
                <xsl:value-of select="$version"/>
                of
                <xsl:value-of select="$name"/>
                is deprecated.
            </div>
        </xsl:if>
        <xsl:if test="not($isLatest)">
            <div class="alert alert-warning">
                Version
                <xsl:value-of select="$version"/>
                <xsl:if test="$revision != 0">
                    <xsl:text>-</xsl:text>
                    <xsl:value-of select="$revision"/>
                </xsl:if>
                of
                <xsl:value-of select="$name"/>
                is out of date.

                <xsl:variable name="theLatest" select="versions/version/latest[normalize-space(.)='true']/.."/>
                <xsl:if test="$theLatest">
                    <xsl:variable name="latestVersion" select="normalize-space($theLatest/number)"/>
                    <xsl:variable name="latestRevision" select="normalize-space($theLatest/revision)"/>
                    <xsl:variable name="latestRoute" select="normalize-space($theLatest/route)"/>

                    The latest version is
                    <a href="{$baseUrl}{$latestRoute}">
                        <xsl:value-of select="$latestVersion"/>
                        <xsl:if test="$latestRevision != 0">
                            <xsl:text>-</xsl:text>
                            <xsl:value-of select="$latestRevision"/>
                        </xsl:if>
                    </a>.
                </xsl:if>
            </div>
        </xsl:if>
        <h2>
            <xsl:value-of select="$name"/>
            <xsl:text> </xsl:text>
            <small class="text-muted">
                <xsl:value-of select="$version"/>
                <xsl:if test="$revision != 0">
                    <xsl:text>-</xsl:text>
                    <xsl:value-of select="$revision"/>
                </xsl:if>
            </small>
        </h2>
        <p>
            View this package on
            <a href="https://hackage.haskell.org/package/{$name}-{$version}">Hackage</a>
            or
            <a href="https://www.stackage.org/package/{$name}">Stacakge</a>.
        </p>
        <dl>
            <dt>name</dt> <dd><xsl:value-of select="$name"/></dd>
            <dt>version</dt> <dd><xsl:value-of select="$version"/></dd>
            <dt>revision</dt> <dd><xsl:value-of select="$revision"/></dd>
            <dt>preferred</dt> <dd><xsl:value-of select="$preferred"/></dd>
            <dt>uploadedAt</dt> <dd><xsl:value-of select="package/uploadedAt"/></dd>
            <dt>uploadedBy</dt> <dd><xsl:value-of select="package/uploadedBy"/></dd>
            <dt>synopsis</dt> <dd><xsl:value-of select="package/synopsis"/></dd>
            <!-- TODO: Render Haddock as HTML. -->
            <dt>description</dt> <dd><xsl:value-of select="package/description"/></dd>
            <dt>license</dt> <dd><xsl:value-of select="package/license"/></dd>
            <dt>author</dt> <dd><xsl:value-of select="package/author"/></dd>
            <dt>copyright</dt> <dd><xsl:value-of select="package/copyright"/></dd>
            <dt>maintainer</dt> <dd><xsl:value-of select="package/maintainer"/></dd>
            <dt>bugReports</dt> <dd><xsl:value-of select="package/bugReports"/></dd>
            <dt>homepage</dt> <dd><xsl:value-of select="package/homepage"/></dd>
            <dt>pkgUrl</dt> <dd><xsl:value-of select="package/pkgUrl"/></dd>
            <dt>buildType</dt> <dd><xsl:value-of select="package/buildType"/></dd>
            <dt>cabalVersion</dt> <dd><xsl:value-of select="package/cabalVersion"/></dd>
            <dt>category</dt> <dd><xsl:value-of select="package/category"/></dd>
            <dt>stability</dt> <dd><xsl:value-of select="package/stability"/></dd>
        </dl>
        <h3>
            Source Repositories
        </h3>
        <ul>
            <xsl:for-each select="sourceRepositories/sourceRepository">
                <li>
                    <dl>
                        <dt>branch</dt> <dd><xsl:value-of select="branch"/></dd>
                        <dt>kind</dt> <dd><xsl:value-of select="kind"/></dd>
                        <dt>location</dt> <dd><xsl:value-of select="location"/></dd>
                        <dt>module</dt> <dd><xsl:value-of select="module"/></dd>
                        <dt>subdir</dt> <dd><xsl:value-of select="subdir"/></dd>
                        <dt>tag</dt> <dd><xsl:value-of select="tag"/></dd>
                        <dt>type</dt> <dd><xsl:value-of select="type"/></dd>
                    </dl>
                </li>
            </xsl:for-each>
        </ul>
        <h3>
            Components
        </h3>
        <ul>
            <xsl:for-each select="components/component">
                <xsl:variable name="componentName" select="normalize-space(name)"/>

                <li>
                    <a href="{$baseUrl}{normalize-space(route)}">
                        <xsl:value-of select="normalize-space(tag)"/>
                        <xsl:if test="$componentName">
                            <xsl:text>:</xsl:text>
                            <xsl:value-of select="$componentName"/>
                        </xsl:if>
                    </a>
                </li>
            </xsl:for-each>
        </ul>
        <h3>
            Versions
        </h3>
        <ul>
            <xsl:for-each select="versions/version">
                <xsl:variable name="versionLatest" select="boolean(normalize-space(latest))"/>
                <xsl:variable name="versionNumber" select="normalize-space(number)"/>
                <xsl:variable name="versionRevision" select="normalize-space(revision)"/>
                <xsl:variable name="versionPreferred" select="boolean(normalize-space(preferred))"/>
                <xsl:variable name="class">
                    <xsl:if test="not($versionPreferred)">
                        text-decoration-line-through
                    </xsl:if>
                    <xsl:if test="$versionLatest">
                        mark
                    </xsl:if>
                    <xsl:if test="$versionNumber = $version and $versionRevision = $revision">
                        text-decoration-none
                    </xsl:if>
                </xsl:variable>
                <xsl:variable name="title">
                    <xsl:text>version </xsl:text>
                    <xsl:value-of select="$versionNumber"/>
                    <xsl:text> revision </xsl:text>
                    <xsl:value-of select="$versionRevision"/>
                    <xsl:if test="not($versionPreferred)">
                        <xsl:text> (deprecated)</xsl:text>
                    </xsl:if>
                    <xsl:if test="$versionLatest">
                        <xsl:text> (latest)</xsl:text>
                    </xsl:if>
                </xsl:variable>

                <li>
                    <code>
                        <xsl:value-of select="normalize-space(uploadedAt)"/>
                    </code>
                    <xsl:text>: </xsl:text>
                    <a class="{$class}" href="{$baseUrl}{normalize-space(route)}" title="{$title}">
                        <xsl:value-of select="$versionNumber"/>
                        <xsl:if test="$versionRevision != 0">
                            <xsl:text>-</xsl:text>
                            <xsl:value-of select="$versionRevision"/>
                        </xsl:if>
                    </a>
                </li>
            </xsl:for-each>
        </ul>
        <h3>
            Files
        </h3>
        <ul>
            <xsl:for-each select="files/file">
                <li>
                    <a href="{$baseUrl}{normalize-space(route)}">
                        <xsl:value-of select="normalize-space(path)"/>
                    </a>
                </li>
            </xsl:for-each>
        </ul>
    </xsl:template>

    <xsl:template match="search">
        <h2>
            Search
        </h2>
        <form action="{$baseUrl}{normalize-space(/root/meta/routes/search)}" class="d-flex mb-3">
            <input class="form-control form-control-lg me-2" name="query" type="search" placeholder="Search" value="{normalize-space(query)}"/>
            <button class="btn btn-lg btn-primary" type="submit">Go</button>
        </form>
        <h3>
            Packages
        </h3>
        <ul>
            <xsl:for-each select="packages/package">
                <li>
                    <a href="{$baseUrl}{normalize-space(route)}">
                        <xsl:value-of select="normalize-space(name)"/>
                    </a>
                </li>
            </xsl:for-each>
        </ul>
    </xsl:template>

    <xsl:template match="account">
        <h2>
            Account
        </h2>
        <p>
            Logged in as
            <a href="https://github.com/{normalize-space(name)}">@<xsl:value-of select="normalize-space(name)"/></a>.
            Manage access
            <a href="https://github.com/settings/connections/applications/{$clientId}">on GitHub</a>.
        </p>
        <form action="{$baseUrl}{normalize-space(/root/meta/routes/logOut)}" method="post">
            <button class="btn btn-outline-danger" type="submit">Log out</button>
        </form>
        <h3>
            Sessions
        </h3>
        <ul>
            <xsl:for-each select="sessions/session">
                <li>
                    <xsl:value-of select="normalize-space(createdAt)"/>:
                    <xsl:value-of select="normalize-space(userAgent)"/>
                    <form action="{$baseUrl}{normalize-space(/root/meta/routes/revoke)}" method="post">
                        <input type="hidden" name="guid" value="{normalize-space(guid)}"/>
                        <button class="btn btn-outline-danger" type="submit">Revoke</button>
                    </form>
                </li>
            </xsl:for-each>
        </ul>
    </xsl:template>

    <xsl:template match="component">
        <dl>
            <dt>package</dt> <dd><xsl:value-of select="package"/></dd>
            <dt>version</dt> <dd><xsl:value-of select="version"/></dd>
            <dt>revision</dt> <dd><xsl:value-of select="revision"/></dd>
            <dt>tag</dt> <dd><xsl:value-of select="tag"/></dd>
            <dt>name</dt> <dd><xsl:value-of select="name"/></dd>
        </dl>
        <h3>
            Modules
        </h3>
        <ul>
            <xsl:for-each select="modules/module">
                <li>
                    <a href="{$baseUrl}{normalize-space(route)}">
                        <xsl:value-of select="normalize-space(name)"/>
                    </a>
                </li>
            </xsl:for-each>
        </ul>
        <h3>
            Dependencies
        </h3>
        <ul>
            <xsl:for-each select="dependencies/dependency">
                <li>
                    <a href="{$baseUrl}{normalize-space(route)}">
                        <xsl:value-of select="normalize-space(packageName)"/>
                    </a>
                    <xsl:if test="libraryName != packageName">
                        <xsl:value-of select="libraryName"/>
                    </xsl:if>
                    <xsl:value-of select="versionRange"/>
                </li>
            </xsl:for-each>
        </ul>
        <h3>
            Reverse Dependencies
        </h3>
        <p>
            See also
            <a href="https://packdeps.haskellers.com/reverse/{normalize-space(package)}">packdeps.haskellers.com</a>.
        </p>
        <ul>
            <xsl:for-each select="reverseDependencies/reverseDependency">
                <li>
                    <a href="{$baseUrl}{normalize-space(route)}">
                        <xsl:value-of select="normalize-space(packageName)"/>
                    </a>
                </li>
            </xsl:for-each>
        </ul>
    </xsl:template>

    <xsl:template match="module">
        <dl>
            <dt> package </dt> <dd> <xsl:value-of select="package"/> </dd>
            <dt> version </dt> <dd> <xsl:value-of select="version"/> </dd>
            <dt> revision </dt> <dd> <xsl:value-of select="revision"/> </dd>
            <dt> component </dt> <dd> <xsl:value-of select="component"/> </dd>
            <dt> module </dt> <dd> <xsl:value-of select="module"/> </dd>
            <dt> file </dt>
            <dd>
                <a href="{$baseUrl}{normalize-space(file/route)}">
                    <xsl:value-of select="normalize-space(file/path)"/>
                </a>
            </dd>
        </dl>
    </xsl:template>
</xsl:stylesheet>
