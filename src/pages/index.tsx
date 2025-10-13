import React from 'react';
import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import styles from './index.module.css';

function HomepageHeader() {
  const { siteConfig } = useDocusaurusContext();
  return (
    <header className={clsx('hero hero--primary', styles.heroBanner)}>
      <div className="container">
        <h1 className="hero__title">{siteConfig.title}</h1>
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        <div className={styles.buttons}>
          <Link className="button button--secondary button--lg" to="/docs/overview">
            Get Started - 5 min ⏱️
          </Link>
        </div>
      </div>
    </header>
  );
}

function FeatureCard({ title, description, icon }) {
  return (
    <div className={clsx('col col--4', styles.feature)}>
      <div className="text--center">
        <div className={styles.featureIcon}>{icon}</div>
      </div>
      <div className="text--center padding-horiz--md">
        <h3>{title}</h3>
        <p>{description}</p>
      </div>
    </div>
  );
}

function HomepageFeatures() {
  const features = [
    {
      title: 'AI-Powered Development',
      icon: '🤖',
      description:
        'Leverage Cursor, ChatGPT Enterprise, and Codex to modernize COBOL applications with AI assistance.',
    },
    {
      title: 'Proven Workflow',
      icon: '🔄',
      description:
        'Follow our Spec → Plan → Code → Review methodology for consistent, high-quality results.',
    },
    {
      title: 'Living Documentation',
      icon: '📚',
      description:
        'Context-aware memory system and comprehensive guides that evolve with your team.',
    },
  ];

  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {features.map((props, idx) => (
            <FeatureCard key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}

function QuickLinks() {
  const links = [
    {
      title: '🎯 Overview',
      description: 'Understand the vision and AI toolkit',
      to: '/docs/overview',
    },
    {
      title: '💡 Cursor IDE',
      description: 'Set up your AI pair programmer',
      to: '/docs/cursor',
    },
    {
      title: '🚀 ChatGPT Enterprise',
      description: 'Master the senior dev assistant',
      to: '/docs/chatgpt',
    },
    {
      title: '⚡ Codex CLI',
      description: 'Deploy your junior dev team',
      to: '/docs/codex',
    },
    {
      title: '📋 Workflow',
      description: 'Learn the development methodology',
      to: '/docs/workflow',
    },
    {
      title: '🔧 Templates',
      description: 'Get starter files and examples',
      to: '/docs/templates',
    },
  ];

  return (
    <section className={styles.quickLinks}>
      <div className="container">
        <h2 className="text--center margin-bottom--lg">Quick Access</h2>
        <div className="row">
          {links.map((link, idx) => (
            <div key={idx} className="col col--4 margin-bottom--md">
              <Link to={link.to} className={clsx('card', styles.quickLinkCard)}>
                <div className="card__header">
                  <h3>{link.title}</h3>
                </div>
                <div className="card__body">
                  <p>{link.description}</p>
                </div>
              </Link>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}

function CallToAction() {
  return (
    <section className={styles.cta}>
      <div className="container">
        <div className="row">
          <div className="col col--12 text--center">
            <h2>Ready to modernize with AI?</h2>
            <p className="margin-bottom--lg">
              Start with our comprehensive overview or dive into specific tools
            </p>
            <div className={styles.ctaButtons}>
              <Link
                className="button button--primary button--lg margin-horiz--sm"
                to="/docs/overview"
              >
                Read Overview
              </Link>
              <Link
                className="button button--secondary button--lg margin-horiz--sm"
                to="/docs/workflow"
              >
                View Workflow
              </Link>
            </div>
          </div>
        </div>
      </div>
    </section>
  );
}

export default function Home() {
  return (
    <Layout
      title={`Home`}
      description="AI-Assisted COBOL Modernization Guide for Global Shop Solutions"
    >
      <HomepageHeader />
      <main>
        <HomepageFeatures />
        <QuickLinks />
        <CallToAction />
      </main>
    </Layout>
  );
}
