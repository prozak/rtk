#!/bin/bash

# Branch Protection Setup Script
# This script helps verify and configure GitHub branch protection rules

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
REQUIRED_CHECK="build-and-test"
BRANCHES=("master" "main" "develop")

echo -e "${BLUE}=== GitHub Branch Protection Setup ===${NC}\n"

# Check if gh CLI is available
if ! command -v gh &> /dev/null; then
    echo -e "${YELLOW}WARNING: GitHub CLI (gh) is not installed.${NC}"
    echo -e "You'll need to set up branch protection manually or install gh CLI."
    echo -e "\nTo install gh: https://cli.github.com/\n"
    echo -e "${BLUE}Manual Setup Instructions:${NC}"
    echo -e "1. Go to GitHub repository Settings > Branches"
    echo -e "2. Add branch protection rule for 'master' (and 'main' if applicable)"
    echo -e "3. Enable: 'Require status checks to pass before merging'"
    echo -e "4. Add required status check: '${REQUIRED_CHECK}'"
    echo -e "5. Enable: 'Require branches to be up to date before merging'"
    echo -e "6. Save changes\n"
    echo -e "See BRANCH_PROTECTION_SETUP.md for detailed instructions."
    exit 0
fi

# Check if authenticated
if ! gh auth status &> /dev/null; then
    echo -e "${RED}ERROR: Not authenticated with GitHub.${NC}"
    echo -e "Please run: ${BLUE}gh auth login${NC}"
    exit 1
fi

# Get repository info
REPO=$(gh repo view --json nameWithOwner -q .nameWithOwner 2>/dev/null)

if [ -z "$REPO" ]; then
    echo -e "${RED}ERROR: Could not determine repository.${NC}"
    echo -e "Make sure you're in a git repository with a GitHub remote."
    exit 1
fi

echo -e "${GREEN}Repository: $REPO${NC}\n"

# Function to check branch protection
check_branch_protection() {
    local branch=$1
    echo -e "${BLUE}Checking branch protection for '$branch'...${NC}"

    # Check if branch exists
    if ! gh api "repos/$REPO/branches/$branch" &> /dev/null; then
        echo -e "${YELLOW}  Branch '$branch' does not exist. Skipping.${NC}\n"
        return
    fi

    # Check protection status
    PROTECTION=$(gh api "repos/$REPO/branches/$branch/protection" 2>/dev/null || echo "none")

    if [ "$PROTECTION" == "none" ]; then
        echo -e "${RED}  ✗ No branch protection enabled${NC}\n"
        return
    fi

    # Check required status checks
    REQUIRED_CHECKS=$(gh api "repos/$REPO/branches/$branch/protection/required_status_checks" 2>/dev/null || echo "none")

    if [ "$REQUIRED_CHECKS" == "none" ]; then
        echo -e "${YELLOW}  ✗ No required status checks configured${NC}\n"
        return
    fi

    # Check if our required check is in the list
    if echo "$REQUIRED_CHECKS" | grep -q "$REQUIRED_CHECK"; then
        echo -e "${GREEN}  ✓ Branch protection enabled${NC}"
        echo -e "${GREEN}  ✓ Required status check '${REQUIRED_CHECK}' is configured${NC}\n"
    else
        echo -e "${YELLOW}  ✓ Branch protection enabled${NC}"
        echo -e "${RED}  ✗ Required status check '${REQUIRED_CHECK}' is NOT configured${NC}\n"
    fi
}

# Function to enable branch protection
enable_branch_protection() {
    local branch=$1
    echo -e "${BLUE}Enabling branch protection for '$branch'...${NC}"

    # Check if branch exists
    if ! gh api "repos/$REPO/branches/$branch" &> /dev/null; then
        echo -e "${YELLOW}  Branch '$branch' does not exist. Skipping.${NC}\n"
        return
    fi

    # Enable branch protection
    gh api -X PUT "repos/$REPO/branches/$branch/protection" \
        -f required_status_checks[strict]=true \
        -f required_status_checks[contexts][]="$REQUIRED_CHECK" \
        -f enforce_admins=true \
        -f required_pull_request_reviews[required_approving_review_count]=1 \
        -f required_pull_request_reviews[dismiss_stale_reviews]=true \
        -f restrictions=null \
        &> /dev/null

    if [ $? -eq 0 ]; then
        echo -e "${GREEN}  ✓ Branch protection enabled successfully${NC}\n"
    else
        echo -e "${RED}  ✗ Failed to enable branch protection${NC}"
        echo -e "${YELLOW}  You may need repository admin permissions${NC}\n"
    fi
}

# Main menu
echo -e "What would you like to do?\n"
echo -e "1) Check current branch protection status"
echo -e "2) Enable branch protection (requires admin permissions)"
echo -e "3) Exit\n"

read -p "Enter your choice [1-3]: " choice

case $choice in
    1)
        echo -e "\n${BLUE}=== Checking Branch Protection Status ===${NC}\n"
        for branch in "${BRANCHES[@]}"; do
            check_branch_protection "$branch"
        done
        ;;
    2)
        echo -e "\n${YELLOW}This will enable branch protection with the following settings:${NC}"
        echo -e "  - Require status checks to pass: ${REQUIRED_CHECK}"
        echo -e "  - Require branches to be up to date"
        echo -e "  - Require pull request reviews: 1 approval"
        echo -e "  - Dismiss stale reviews when new commits are pushed"
        echo -e "  - Enforce for administrators\n"

        read -p "Continue? [y/N]: " confirm
        if [[ $confirm =~ ^[Yy]$ ]]; then
            echo ""
            for branch in "${BRANCHES[@]}"; do
                enable_branch_protection "$branch"
            done
        else
            echo -e "\n${YELLOW}Cancelled.${NC}"
        fi
        ;;
    3)
        echo -e "\n${BLUE}Exiting.${NC}"
        exit 0
        ;;
    *)
        echo -e "\n${RED}Invalid choice.${NC}"
        exit 1
        ;;
esac

echo -e "${GREEN}Done!${NC}"
