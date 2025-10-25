# Branch Protection Setup Guide

## Overview
This guide explains how to configure GitHub branch protection rules to prevent merging code without CI passing.

## Current CI Setup
- **Workflow Name:** CI
- **Job Name:** build-and-test
- **Protected Branches:** master, main, develop

## Manual Setup (GitHub Web UI)

### Step 1: Navigate to Settings
1. Go to your GitHub repository
2. Click on **Settings** tab
3. In the left sidebar, click **Branches** (under "Code and automation")

### Step 2: Add Branch Protection Rule
1. Click **Add rule** or **Add branch protection rule**
2. In "Branch name pattern" enter: `master` (or `main` if that's your default)
   - You can use `master` for the master branch
   - You can use `main` for the main branch
   - You can use `develop` for the develop branch
   - Or use `*` to protect all branches

### Step 3: Configure Protection Rules
Enable the following settings:

#### Required Settings
- ✅ **Require a pull request before merging**
  - ✅ Require approvals: 1 (optional, but recommended)
  - ✅ Dismiss stale pull request approvals when new commits are pushed (recommended)

- ✅ **Require status checks to pass before merging** ⚠️ **CRITICAL**
  - ✅ Require branches to be up to date before merging (recommended)
  - **Add required status checks:**
    - Search for and select: `build-and-test` (this is your CI job name)
    - If it doesn't appear, you need to run the CI at least once on a PR first

#### Additional Recommended Settings
- ✅ **Require conversation resolution before merging** (optional)
- ✅ **Do not allow bypassing the above settings** (prevents admins from bypassing)
- ✅ **Restrict who can push to matching branches** (optional - for high security)

### Step 4: Save
1. Scroll down and click **Create** or **Save changes**

## Using GitHub CLI (Alternative Method)

If you have `gh` CLI installed and authenticated:

```bash
# Protect master branch
gh api repos/{owner}/{repo}/branches/master/protection \
  --method PUT \
  --field required_status_checks='{"strict":true,"contexts":["build-and-test"]}' \
  --field enforce_admins=true \
  --field required_pull_request_reviews='{"required_approving_review_count":1}' \
  --field restrictions=null

# Protect main branch (if applicable)
gh api repos/{owner}/{repo}/branches/main/protection \
  --method PUT \
  --field required_status_checks='{"strict":true,"contexts":["build-and-test"]}' \
  --field enforce_admins=true \
  --field required_pull_request_reviews='{"required_approving_review_count":1}' \
  --field restrictions=null
```

Replace `{owner}` and `{repo}` with your repository details.

## Using GitHub REST API

You can also use curl with a personal access token:

```bash
curl -X PUT \
  -H "Accept: application/vnd.github.v3+json" \
  -H "Authorization: token YOUR_GITHUB_TOKEN" \
  https://api.github.com/repos/{owner}/{repo}/branches/master/protection \
  -d '{
    "required_status_checks": {
      "strict": true,
      "contexts": ["build-and-test"]
    },
    "enforce_admins": true,
    "required_pull_request_reviews": {
      "required_approving_review_count": 1,
      "dismiss_stale_reviews": true
    },
    "restrictions": null
  }'
```

## Verification

After setting up branch protection:

1. Create a test branch
2. Make a small change
3. Create a pull request to master
4. Try to merge before CI completes - it should be blocked
5. Wait for CI to pass - merge button should become available
6. If CI fails - merge button should remain disabled

## Key Points

- The status check name **must exactly match** the job name in your workflow: `build-and-test`
- You need to run CI at least once before the status check appears in the dropdown
- Branch protection rules apply to everyone, including repository admins (if "Do not allow bypassing" is enabled)
- You can create separate rules for different branch patterns

## Troubleshooting

### Status check doesn't appear in the list
- Run the CI workflow at least once on a pull request
- Refresh the branch protection settings page
- The check name must match exactly: `build-and-test`

### Can still merge without CI
- Verify the status check is added to required checks
- Check if "Do not allow bypassing" is enabled
- Ensure you're testing on the protected branch

### CI is skipped on PRs
- Check the workflow trigger configuration
- Ensure `pull_request` trigger includes your branch
- Check if there are any path filters excluding your changes

## Current Workflow Configuration

Your CI workflow (`.github/workflows/ci.yml`) triggers on:
- Push to: main, master, develop
- Pull requests to: main, master, develop

The workflow includes one job: `build-and-test`

This is the status check name you need to add to required status checks.
