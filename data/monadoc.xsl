<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output
        doctype-system="about:legacy-compat"
        encoding="UTF-8"
        media-type="text/html"
        method="html"/>

    <xsl:variable name="baseUrl" select="normalize-space(/monadoc/config/baseUrl)"/>
    <xsl:variable name="clientId" select="normalize-space(/monadoc/config/clientId)"/>

    <xsl:template match="/monadoc">
        <html lang="en-US">
            <head>
                <meta content="initial-scale = 1, width = device-width" name="viewport"/>
                <title>
                    Monadoc
                </title>
                <link href="{$baseUrl}{normalize-space(config/routes/bootstrap)}" rel="stylesheet"/>
                <link
                    href="{$baseUrl}{normalize-space(config/routes/favicon)}"
                    rel="icon"
                    type="image/svg+xml"/>
                <link rel="canonical" href="{$baseUrl}{normalize-space(config/routes/self)}"/>
            </head>
            <body>
                <header class="mb-3">
                    <nav class="navbar navbar-dark bg-dark flex-nowrap">
                        <div class="container">
                            <a class="navbar-brand" href="{$baseUrl}">Monadoc</a>
                            <form action="{$baseUrl}{normalize-space(config/routes/search)}" class="d-flex">
                                <input class="form-control me-1" name="query" type="search" placeholder="Search"/>
                                <button class="btn btn-outline-light" type="submit">Go</button>
                            </form>
                        </div>
                    </nav>
                    <nav class="navbar navbar-light bg-light">
                        <div class="container">
                            <ol class="breadcrumb mb-0">
                                <xsl:for-each select="config/breadcrumbs/breadcrumb">
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
                                    <xsl:variable name="user" select="normalize-space(config/user)"/>
                                    <xsl:choose>
                                        <xsl:when test="$user">
                                            <a class="nav-link" href="{$baseUrl}{normalize-space(config/routes/account)}">@<xsl:value-of select="$user"/></a>
                                        </xsl:when>
                                        <xsl:otherwise>
                                            <a class="nav-link" href="https://github.com/login/oauth/authorize?client_id={$clientId}&amp;redirect_uri={$baseUrl}{normalize-space(config/routes/callback)}">Log in</a>
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
                <footer class="container pt-3 mt-3 text-muted border-top">
                    <p>
                        Powered by
                        <a href="https://github.com/tfausak/monadoc">Monadoc</a>
                        version
                        <xsl:value-of select="normalize-space(config/version)"/>.
                    </p>
                </footer>
            </body>
        </html>
    </xsl:template>

    <xsl:template match="index">
        <p>
            &#x1f516; Better Haskell documentation.
        </p>
        <h2>
            Recent Uploads
        </h2>
        <ul>
            <xsl:for-each select="packages/package">
                <li>
                    <code>
                        <xsl:value-of select="normalize-space(uploadedAt)"/>
                    </code>
                    <xsl:text>: </xsl:text>
                    <a href="{$baseUrl}{normalize-space(route)}">
                        <xsl:value-of select="normalize-space(name)"/>
                        <xsl:text>-</xsl:text>
                        <xsl:value-of select="normalize-space(version)"/>
                        <xsl:text>-</xsl:text>
                        <xsl:value-of select="normalize-space(revision)"/>
                    </a>
                </li>
            </xsl:for-each>
        </ul>
    </xsl:template>

    <xsl:template match="revision">
        <xsl:variable name="name" select="normalize-space(package/name)"/>
        <xsl:variable name="version" select="normalize-space(package/version)"/>
        <xsl:variable name="preferred" select="boolean(normalize-space(package/preferred))"/>

        <xsl:if test="not($preferred)">
            <div class="alert alert-warning">
                Version
                <xsl:value-of select="$version"/>
                of
                <xsl:value-of select="$name"/>
                is deprecated.
            </div>
        </xsl:if>
        <h2>
            <xsl:value-of select="$name"/>
            <xsl:text> </xsl:text>
            <small class="text-muted">
                <xsl:value-of select="$version"/>
                <xsl:text>-</xsl:text>
                <xsl:value-of select="normalize-space(package/revision)"/>
            </small>
        </h2>
        <p>
            View this package <a href="https://hackage.haskell.org/package/{$name}-{$version}">on Hackage</a>.
        </p>
        <ul>
            <li> name: <xsl:value-of select="package/name"/> </li>
            <li> version: <xsl:value-of select="package/version"/> </li>
            <li> revision: <xsl:value-of select="package/revision"/> </li>
            <li> preferred: <xsl:value-of select="$preferred"/> </li>
            <li> uploadedAt: <xsl:value-of select="package/uploadedAt"/> </li>
            <li> uploadedBy: <xsl:value-of select="package/uploadedBy"/> </li>
            <li> synopsis: <xsl:value-of select="package/synopsis"/> </li>
            <li> description: <xsl:value-of select="package/description"/> </li>

            <li> license: <xsl:value-of select="package/license"/> </li>
            <li> author: <xsl:value-of select="package/author"/> </li>
            <li> copyright: <xsl:value-of select="package/copyright"/> </li>
            <li> maintainer: <xsl:value-of select="package/maintainer"/> </li>

            <li> bugReports: <xsl:value-of select="package/bugReports"/> </li>
            <li> homepage: <xsl:value-of select="package/homepage"/> </li>
            <li> pkgUrl: <xsl:value-of select="package/pkgUrl"/> </li>

            <li> buildType: <xsl:value-of select="package/buildType"/> </li>
            <li> cabalVersion: <xsl:value-of select="package/cabalVersion"/> </li>
            <li> category: <xsl:value-of select="package/category"/> </li>
            <li> stability: <xsl:value-of select="package/stability"/> </li>
        </ul>
        <h3>
            Components
        </h3>
        <ul>
            <xsl:for-each select="components/component">
                <xsl:variable name="componentName" select="normalize-space(name)"/>

                <li>
                    <xsl:value-of select="normalize-space(tag)"/>
                    <xsl:if test="$componentName">
                        <xsl:text>:</xsl:text>
                        <xsl:value-of select="$componentName"/>
                    </xsl:if>
                </li>
            </xsl:for-each>
        </ul>
        <h3>
            Versions
        </h3>
        <ul>
            <xsl:for-each select="versions/version">
                <xsl:variable name="versionPreferred" select="boolean(normalize-space(preferred))"/>

                <li>
                    <code>
                        <xsl:value-of select="normalize-space(uploadedAt)"/>
                    </code>
                    <xsl:text>: </xsl:text>
                    <a href="{$baseUrl}{normalize-space(route)}">
                        <xsl:value-of select="normalize-space(number)"/>
                        <xsl:text>-</xsl:text>
                        <xsl:value-of select="normalize-space(revision)"/>
                    </a>
                    <xsl:if test="not($versionPreferred)">
                        (deprecated)
                    </xsl:if>
                </li>
            </xsl:for-each>
        </ul>
    </xsl:template>

    <xsl:template match="search">
        <h2>
            Search
        </h2>
        <form action="{$baseUrl}{normalize-space(/monadoc/config/routes/search)}" class="d-flex mb-3">
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
        <form action="{$baseUrl}{normalize-space(/monadoc/config/routes/logOut)}" method="post">
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
                    <form action="{$baseUrl}{normalize-space(/monadoc/config/routes/revoke)}" method="post">
                        <input type="hidden" name="guid" value="{normalize-space(guid)}"/>
                        <button class="btn btn-outline-danger" type="submit">Revoke</button>
                    </form>
                </li>
            </xsl:for-each>
        </ul>
    </xsl:template>
</xsl:stylesheet>
